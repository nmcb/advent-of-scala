package aoc2020

import nmcb.*

object Day25 extends AoC:

  def solve(card: Int, door: Int): Int =
    def step(n: Int): Int = (n * 7) % 20201227
    BigInt(card).modPow(Iterator.iterate(1)(step).indexWhere(_ == door), 20201227).toInt

  lazy val answer1: Int    = solve(card = 5290733, door = 15231938)
  lazy val answer2: String = "<unimplemented>"
