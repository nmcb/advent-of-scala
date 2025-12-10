package aoc2020

import nmcb.*
import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

object Day09 extends AoC:

  val (preamble, inbound) = lines.map(_.toLong).splitAt(25)

  def solve1(preamble: Vector[Long], inbound: Vector[Long]): Long =
    @tailrec
    def go(todo: Vector[Long], prem: Vector[Long] = preamble): Long =
      todo match
        case test +: rest =>
          if prem.combinations(2).map(_.sum).contains(test) then
            go(rest, prem.drop(1) :+ test)
          else
            test
        case _            => sys.error("boom")
    go(inbound)

  def solve2(inbound: Vector[Long], sum: Long): Option[Long] =
    var result: Option[Long] = None
    boundary:
      for size <- 2 to inbound.length do
        for test <- inbound.sliding(size) do
          if test.sum == sum then
            result = Some(test.max + test.min)
            break(result)
        if result.isDefined then
          break(result)
    result

  lazy val answer1: Long = solve1(preamble, inbound)
  lazy val answer2: Long = solve2(inbound, answer1).get
