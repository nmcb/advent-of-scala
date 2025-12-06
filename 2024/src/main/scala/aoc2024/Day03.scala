package aoc2024

import nmcb.*

import scala.io.*

object Day03 extends AoC:

  private val input: String =
    Source.fromResource(s"$day.txt").mkString

  def solve(memory: String, result: Int, enabled: Boolean, part2: Boolean): Int =
    memory match
      case "" =>
        result
      case s"mul($l,$r)$rest" if l.toIntOption.nonEmpty && r.toIntOption.nonEmpty && enabled =>
        solve(rest, result + l.toInt * r.toInt, enabled, part2)
      case s"don't()$rest" if part2 =>
        solve(rest, result, false, part2)
      case s"do()$rest" if part2 =>
        solve(rest, result, true, part2)
      case _ =>
        solve(memory.tail, result, enabled, part2)


  val answer1: Int = solve(memory = input, result = 0, enabled = true, part2 = false)
  val answer2: Int = solve(memory = input, result = 0, enabled = true, part2 = true)
