import Day05.moves

import scala.io.*
import scala.util.*

object Day05 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  case class Move(size: Int, from: Int, to: Int)

  val input: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  def isLabelLine(line: String): Boolean =
    line.trim.nonEmpty && line.trim.head.isDigit

  def isStackLine(line: String): Boolean =
    line.dropWhile(_ == ' ').startsWith("[")

  def isCommandLine(line: String): Boolean =
    line.startsWith("move")

  val stack: List[List[Char]] =
    val columnSize: Int =
      input.filter(isLabelLine) match
        case List(cols) => cols.split(' ').last.toInt
        case _ => sys.error("multiple label lines")
    def crateExtractor(line: String)(idx: Int): Option[Char] =
      Try(line.charAt(1 + idx * 4)) match
        case Success(c) if c.isUpper => Some(c)
        case _ => None
    def parser(line: String): List[Option[Char]] =
      List.tabulate(columnSize)(crateExtractor(line))
    input
      .filter(isStackLine)
      .map(parser)
      .transpose
      .map(_.flatten)

  val moves: List[Move] =
    input
      .filter(isCommandLine)
      .map { case s"move $s from $f to $t" => Move(s.toInt, f.toInt - 1, t.toInt - 1) }


  def run(moves: List[Move], stacks: List[List[Char]], place: List[Char] => List[Char]): String =
    if moves.isEmpty then
      stacks.map(_.head).mkString("")
    else
      val m  = moves.head
      val md = stacks(m.from).take(m.size)
      val mf = stacks(m.from).drop(m.size)
      val mt = place(md) ++ stacks(m.to)
      val ns = stacks.updated(m.from, mf).updated(m.to, mt)
      run(moves.tail, ns, place)

  val answer1: String =
    run(moves, stack, _.reverse)

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  val answer2: String =
    run(moves, stack, identity)

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
