package aoc2022

import nmcb.*

import scala.annotation.tailrec
import scala.io.*

object Day25 extends AoC:

  val snafus: List[Number] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .map(s => Number.fromString(s.toList))
      .toList

  case class Digit(c: Char):

    def toLong: Long =
      c match
        case '2' =>  2L
        case '1' =>  1L
        case '0' =>  0L
        case '-' => -1L
        case '=' => -2L

    override def toString: String =
      s"${c.toString}"


  object Digit:
    val Powers: Long = 5

    def fromChar(c: Char): Digit =
      c match
        case '2' | '1' | '0' | '-' | '=' => Digit(c)
        case _ => sys.error(s"boom: '$c'")

  case class Number(digits: List[Digit]):

    def toLong: Long =
      digits.reverse.foldLeft((0L,1L)) { case ((a, p), d) =>
        (a + (p * d.toLong), 5L * p)
      }._1

    override def toString: String =
      val l = toLong
      s"${digits.mkString("")}"

  object Number:

    @tailrec
    def fromString(s: List[Char], a: List[Digit] = List.empty): Number =
      s match
        case Nil    => Number(a)
        case h :: t => fromString(t, a :+ Digit.fromChar(h))

    @tailrec
    def fromLong(l: Long, a: List[Digit] = List.empty): Number =
      val remainder = l % 5

      val dividend = remainder match
        case 0 | 1 | 2 => l / 5
        case 3         => (l + 2) / 5
        case 4         => (l + 1) / 5

      val acc = remainder match
        case 0 | 1 | 2 => Digit.fromChar(remainder.toString.head) :: a
        case 3         => Digit.fromChar('=') :: a
        case 4         => Digit.fromChar('-') :: a

      if dividend == 0 then Number(acc) else fromLong(dividend, acc)


  lazy val answer1: String = Number.fromLong(snafus.map(_.toLong).sum).toString
  lazy val answer2: String = "<unimplemented>"
