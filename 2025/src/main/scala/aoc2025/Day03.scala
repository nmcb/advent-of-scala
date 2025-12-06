package aoc2025

import nmcb.*

import scala.io.*

object Day03 extends AoC:

  type Bank = String
  val input: Vector[Bank] =
      Source
        .fromResource(s"$day.txt")
        .getLines
        .toVector

  extension (bank: Bank)

    def maxJoltage(digits: Int): Long =
      @annotation.tailrec
      def loop(todo: String, remaining: Int, accumulator: Long = 0): Long =
        if remaining == 0 then
          accumulator
        else
          val until = todo.length - remaining + 1
          val digit = todo.substring(0, until).max
          val index = todo.indexOf(digit)
          loop(todo.substring(index + 1), remaining - 1, accumulator * 10 + digit.asDigit)

      loop(bank, digits)

  lazy val answer1: Long = input.map(_.maxJoltage(2)).sum
  lazy val answer2: Long = input.map(_.maxJoltage(12)).sum
