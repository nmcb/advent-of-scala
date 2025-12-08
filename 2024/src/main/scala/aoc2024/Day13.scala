package aoc2024

import nmcb.*

import scala.math.Integral.Implicits.*

object Day13 extends AoC:

  case class Pos(x: Long, y: Long)

  case class Machine(a: Pos, b: Pos, p: Pos):

    def cost(m: Long, n: Long): Long =
      m * 3 + n

    def solve(offset: Long): Option[Long] =
      val px       = p.x + offset
      val py       = p.y + offset
      val div      = a.x * b.y - b.x * a.y
      val (ma, ra) = (px * b.y - py * b.x) /% div
      val (nb, rb) = (py * a.x - px * a.y) /% div
      if ra == 0 & rb == 0 then Some(cost(ma, nb)) else None

  val machines: Vector[Machine] =
    val as = lines.collect:
      case s"Button A: X+$x, Y+$y" => Pos(x.toLong, y.toLong)
    val bs = lines.collect:
      case s"Button B: X+$x, Y+$y" => Pos(x.toLong, y.toLong)
    val ps = lines.collect:
      case s"Prize: X=$x, Y=$y"    => Pos(x.toLong, y.toLong)
    as.zip(bs).zip(ps).map:
      case ((a,b),p) => Machine(a, b, p)

  lazy val answer1: Long = machines.flatMap(_.solve(offset = 0L)).sum
  lazy val answer2: Long = machines.flatMap(_.solve(offset = 10000000000000L)).sum
