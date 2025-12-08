package aoc2021

import nmcb.*

object Day01 extends AoC:

  val depths: Vector[Int] = lines.map(_.toInt)
  
  def solve(l: Vector[Int]): Int = l.sliding(2).count(l => l(1) > l(0))

  lazy val answer1: Int = solve(depths)
  lazy val answer2: Int = solve(depths.sliding(3).map(_.sum).toVector)
