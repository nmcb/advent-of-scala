package aoc2020

import nmcb.*

object Day01 extends AoC:

  def answer(numberOfArgs: Int, sumEquals: Int): Int =
    lines
      .map(_.toInt)
      .combinations(numberOfArgs)
      .filter(_.sum == sumEquals)
      .map(_.product)
      .next


  lazy val answer1: Int = answer(numberOfArgs = 2, sumEquals = 2020)
  lazy val answer2: Int = answer(numberOfArgs = 3, sumEquals = 2020)
