package aoc2020

import nmcb.*

object Day15 extends AoC:

  val numbers: Vector[Int] = "5,1,9,18,13,8,0".split(',').map(_.toInt).toVector

  def solve(numbers: Vector[Int], turns: Int): Int =
    val map = numbers.init.zipWithIndex.toMap
    val (_, result) = (numbers.length until turns).foldLeft((map,numbers.last)):
      case ((map,last),index) if map.contains(last) => (map.updated(last, index - 1), index - 1 - map(last))
      case ((map,last),index)                       => (map.updated(last, index - 1), 0)
    result


  lazy val answer1: Int = solve(numbers, 2020)
  lazy val answer2: Int = solve(numbers, 30000000)
