package aoc2022

import nmcb.*

import scala.io.*
import scala.util.*

object Day05 extends AoC:

  case class Move(size: Int, from: Int, to: Int)

  val input: List[String] =
    Source
      .fromResource(s"$day.txt")
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


  lazy val answer1: String = run(moves, stack, _.reverse)
  lazy val answer2: String = run(moves, stack, identity)
