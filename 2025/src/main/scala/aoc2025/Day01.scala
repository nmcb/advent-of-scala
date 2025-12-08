package aoc2025

import nmcb.*

object Day01 extends AoC:

  def solve1(input: Vector[String]): Int =
    input
      .scanLeft(50): (last, rotation) =>
        rotation match
          case s"R$count" => last + count.toInt
          case s"L$count" => last - count.toInt
      .count(step => step % 100 == 0)

  def solve2(input: Vector[String]): Int =
    input.flatMap:
          case s"R$count" => Vector.fill(count.toInt)(1)
          case s"L$count" => Vector.fill(count.toInt)(-1)
          case code       => sys.error(s"invalid code: $code")
      .scanLeft(50)(_ + _)
      .count(step => step % 100 == 0)

  lazy val answer1: Int = solve1(lines)
  lazy val answer2: Int = solve2(lines)
