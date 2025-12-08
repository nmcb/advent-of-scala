package aoc2022

import nmcb.*

import scala.annotation.tailrec

object Day06 extends AoC:

  case class Move(size: Int, from: Int, to: Int)

  def solve(s: String, d: Int): Int =
    @tailrec
    def loop(todo: String, last: String, count: Int = d): Int =
      if todo.isEmpty then sys.error("not found")
      else
        val c = todo.head
        val l = (last + c).drop(1)
        if   count >= d && l.distinct.size == d then count + 1
        else loop(todo.tail, l, count + 1)
    loop(s.drop(d), s.take(d))

  def solve1(s: String): Int =
    solve(s, 4)

  def solve2(s: String): Int =
    solve(s, 14)


  lazy val answer1: Int = solve1(input)
  lazy val answer2: Int = solve2(input)
