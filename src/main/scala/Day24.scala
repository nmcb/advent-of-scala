import Day24.Name
import nmcb.*
import nmcb.predef.*

import scala.io.*

object Day24 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  type Wire = String

  sealed trait Expr:
    def evaluate(delegate: Map[Wire,Expr]): Boolean

  case class Bool(v: Boolean) extends Expr:
    override def evaluate(delegate: Map[Wire,Expr]) =
      v

  case class Name(n: Wire) extends Expr:
    override def evaluate(delegate: Map[Wire,Expr]) =
      delegate(n).evaluate(delegate)

  case class Gate(l: Expr, r: Expr, op: String) extends Expr:
    override def evaluate(delegate: Map[Wire,Expr]) =
      op match
        case "AND" => l.evaluate(delegate) && r.evaluate(delegate)
        case "OR"  => l.evaluate(delegate) || r.evaluate(delegate)
        case "XOR" => l.evaluate(delegate) ^  r.evaluate(delegate)


  val expressions: Map[Wire,Expr] =
    val input = Source.fromResource(s"input$day.txt").getLines.toVector
    val wires = input.collect:
      case s"$k: $v" => k -> Bool(v.toInt >= 1)
    val gates = input.collect:
      case s"$lhs $op $rhs -> $out" => out -> Gate(Name(lhs), Name(rhs), op)
    (wires ++ gates).toMap


  extension (es: (Wire,Expr))
    def wire: Wire = es._1
    def expr: Expr = es._2


  extension (s: Vector[Boolean])
    def toLong: Long =
      val (multiplier, result) = s.foldLeft((1L, 0L)):
        case ((m,r),d) => if d then  (m * 2, r + m) else (m * 2, r)
      result

  def compute(expressions: Map[Wire, Expr], prefix: Wire): Long =
    expressions
      .filter(_.wire.startsWith(prefix))
      .toVector
      .sortBy(_.wire)
      .map(_.expr.evaluate(expressions))
      .toLong

  val start1: Long = System.currentTimeMillis
  val answer1: Long = compute(expressions, "z")
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  extension (expressions: Map[Wire,Expr])

    def patch(w1: Wire, w2: Wire): Map[Wire,Expr] =
      expressions.updated(w1, expressions(w2)).updated(w2, expressions(w1))

    def findOutputUsing(wire: Wire): Wire =
      val uses = expressions
        .filter: e =>
          e.expr match
            case Gate(Name(`wire`), _, _) | Gate(_, Name(`wire`), _) => true
            case _                                                   => false
      uses
        .find: e =>
          e.wire.startsWith("z")
        .map: e =>
          "z" + (e.wire.tail.toInt - 1).toString.leftPadTo(2, '0')
        .getOrElse(uses.keySet.map(w => expressions.findOutputUsing(w)).min)


    def checkAdder(suffixes: Iterable[Wire]): Map[Wire,Expr] =
      expressions
        .filter: e =>
          e.wire.startsWith("z") && suffixes.exists(e.wire.endsWith)
        .filterNot: e =>
          e.expr match
            case Gate(_, _, "XOR") => true
            case _ => false

    def checkCarry(suffixes: Vector[Wire]): Map[Wire,Expr] =
      expressions
        .filter: e =>
          !e.wire.startsWith("z")
        .filter: e =>
          e.expr match
            case Gate(Name(l), _, _) if l.startsWith("x") || l.startsWith("y") => false
            case Gate(_, Name(r), _) if r.startsWith("x") || r.startsWith("y") => false
            case Gate(_, _, "XOR")                                             => true
            case _                                                             => false

    def checkBits: Map[Wire,Expr] =
      val x = compute(expressions, "x")
      val y = compute(expressions, "y")
      val z = compute(expressions, "z")
      println(s"x=$x, y=$y, =$z")
      val diff = z ^ (x + y)
      println(diff)
      val bits = (diff.toBinaryString.length - 1).toString.leftPadTo(2, '0')
      println(bits)
      expressions
        .filter: e =>
          e.expr match
            case Gate(Name(x), Name(y), _) if x.endsWith(bits) && y.endsWith(bits) => true
            case _                                                                 => false


  def bugged(expressions: Map[Wire,Expr]): Set[Wire] =
    val suffixes = expressions.keys.filter(_.startsWith("x")).map(_.tail).toVector
    val adder    = expressions.checkAdder(suffixes)
    val carries  = expressions.checkCarry(suffixes)
    val patched  = carries.foldLeft(expressions)((es,e) => es.patch(e.wire, es.findOutputUsing(e.wire)))
    val bits     = patched.checkBits
    adder.keySet ++ carries.keySet ++ bits.keySet


  val start2: Long = System.currentTimeMillis
  val answer2: String = bugged(expressions).toVector.sorted.mkString(",")
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
