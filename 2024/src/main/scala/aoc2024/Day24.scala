package aoc2024

import nmcb.*

import scala.annotation.*


/** visual inspection - then coded recognizers - see /doc/img/visual-inspection-day25.pdf */
object Day24 extends AoC:

  type Wire = String

  sealed trait Gate:
    def lhs: Wire
    def rhs: Wire
    def out: Wire

    def isAND: Boolean =
      this match
        case a: Gate.AND => true
        case _           => false

    def isOR: Boolean =
      this match
        case a: Gate.OR => true
        case _          => false

    def isXOR: Boolean =
      this match
        case a: Gate.XOR => true
        case _           => false

  object Gate:
    case class AND(lhs: Wire, rhs: Wire, out: Wire) extends Gate
    case class  OR(lhs: Wire, rhs: Wire, out: Wire) extends Gate
    case class XOR(lhs: Wire, rhs: Wire, out: Wire) extends Gate

  import Gate.*

  @tailrec
  def solve(gates: Vector[Gate], wires: Map[Wire,Boolean]): Map[Wire,Boolean] =
    if gates.nonEmpty then
      val o = gates.head
      val r = gates.tail
      val v = o match
        case AND(l, r, _) => wires(l) && wires(r)
        case OR (l, r, _) => wires(l) || wires(r)
        case XOR(l, r, _) => wires(l) != wires(r)
      solve(r, wires + (o.out -> v))
    else
      wires

  extension (results: Map[Wire, Boolean])
    def output: Long =
      val number: String =
        results
          .toVector
          .filter: r =>
            r.wire.startsWith("z")
          .sortBy(_.wire)
          .map: r =>
            if r.value then "1" else "0"
          .mkString
          .reverse
      BigInt(number, 2).toLong

  extension (w: Wire)
    def asNumber: String = w.filter(_.isDigit)

  extension (result: (Wire,Boolean))
    def wire: Wire     = result._1
    def value: Boolean = result._2

  def sort(gates: Vector[Gate], wires: Map[Wire,Boolean]): Vector[Gate] =
    @tailrec
    def loop(todo: Vector[Gate], result: Vector[Gate], resolved: Set[String]): Vector[Gate] =
      if todo.nonEmpty then
        val g = todo.head
        val r = todo.tail
        if resolved.contains(g.out) then
          loop(r, result, resolved)
        else if resolved.contains(g.lhs) && resolved.contains(g.rhs) then
          loop(r, result :+ g, resolved + g.out)
        else
          val dependencies = todo.filter(op => op.out == g.lhs || op.out == g.rhs)
          loop(dependencies ++ todo, result, resolved)
      else
        result

    loop(gates, Vector.empty, wires.keySet)

  def debugOUT(gates: Vector[Gate]): Set[Wire] =
    gates
      .filter(_.out.startsWith("z"))
      .filter(!_.isXOR)
      .map(_.out)
      .filter(_ != "z45")
      .toSet

  def debugAND(gates: Vector[Gate]): Set[Wire] =
    gates
      .filter: op =>
        op.isAND && op.lhs.asNumber != "00" && (op.lhs.startsWith("x") || op.rhs.startsWith("x"))
      .filter: op =>
        !gates.exists: op2 =>
          (op2.lhs == op.out || op2.rhs == op.out) && op2.isOR
      .map(_.out)
      .toSet

  def debugXOR(gates: Vector[Gate]): Vector[Wire] =
    gates
      .filter: op =>
        (op.lhs.startsWith("x") || op.rhs.startsWith("x")) && op.isXOR && op.lhs.asNumber != "00"
      .flatMap: op =>
        gates.find(g => (g.lhs == op.out || g.rhs == op.out) && g.isXOR) match
          case Some(xor) => if xor.out == s"z${op.lhs.asNumber}" then None else Some(xor.out)
          case None      => Some(op.out)

  val (initial, gates) =
    val initial =
      lines.collect:
        case s"$w: $v" => w -> (v == "1")
      .toMap

    val operations =
      lines.collect[Gate]:
        case s"$t1 XOR $t2 -> $out" => Gate.XOR(t1, t2, out)
        case s"$t1 OR $t2 -> $out"  => Gate.OR(t1, t2, out)
        case s"$t1 AND $t2 -> $out" => Gate.AND(t1, t2, out)

    (initial, operations)

  lazy val answer1: Long = solve(sort(gates, initial), initial).output
  lazy val answer2: String = (debugOUT(gates) ++ debugAND(gates) ++ debugXOR(gates)).toVector.sorted.mkString(",")
