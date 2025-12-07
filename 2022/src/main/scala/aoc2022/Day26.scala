package aoc2022

import nmcb.*
import scala.annotation.tailrec

object Day26 extends AoC:

  type LookAndSay = Vector[Char]

  def decompose(number: LookAndSay): Vector[LookAndSay] =
    @tailrec
    def loop(number: LookAndSay, digits: Vector[LookAndSay]): Vector[LookAndSay] =
      number match
        case h +: t if digits.last.contains(h) => loop(t, digits.init :+ (digits.last :+ h))
        case h +: t                            => loop(t, digits :+ (Vector.empty :+ h))
        case _                                 => digits
    if number.isEmpty then Vector.empty else loop(number.tail, Vector(Vector(number.head)))

  def successor(n: LookAndSay): LookAndSay =
    decompose(n).flatMap(c => Vector(c.size.toString.head, c.head))


  lazy val answer1: Int    = Iterator.iterate(Vector('1'))(successor).take(10).toList.last.size
  lazy val answer2: String = "<unimplemented>"