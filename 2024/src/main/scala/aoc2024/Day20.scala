package aoc2024

import nmcb.*
import nmcb.pos.*

import scala.Option.*
import scala.annotation.*

object Day20 extends AoC:

  def cheats(path: Vector[Pos], timeframe: Long): Long =
    @tailrec
    def trace(trail: Vector[(Pos,Int)], cheated: Long): Long =
      if trail.nonEmpty then
        val (from,time) = trail.head
        val rest        = trail.tail
        val saved = rest.flatMap((to,left) => when(from.manhattan(to) <= timeframe)(left - time - from.manhattan(to)))
        trace(rest, cheated + saved.count(_ >= 100))
      else
        cheated

    trace(path.zipWithIndex, 0L)

  val path: Vector[Pos] = Grid.fromLines(lines).extractPath('S', 'E', '.').shortest

  lazy val answer1: Long = cheats(path, 2)
  lazy val answer2: Long = cheats(path, 20)
