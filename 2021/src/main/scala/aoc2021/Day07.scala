package aoc2021

import nmcb.*

object Day07 extends AoC:

  val positions: Vector[Int] =
    input
      .split(",")
      .map(_.toInt)
      .toVector

  extension (positions: Vector[Int])

    def fuelConsumptionTo1(target: Int): Int =
      positions.map(pos => (target - pos).abs).sum

    def fuelConsumptionTo2(target: Int): Int =
      positions.map(pos => (1 to (target - pos).abs).sum).sum

    def solve(fuelTo: Vector[Int] => Int => Int): Int =
      (positions.min to positions.max).map(fuelTo(positions)).min


  lazy val answer1: Int = positions.solve(_.fuelConsumptionTo1)
  lazy val answer2: Int = positions.solve(_.fuelConsumptionTo2)
