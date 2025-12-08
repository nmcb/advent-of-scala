package aoc2021

import nmcb.*
import nmcb.predef.*

object Day06 extends AoC:

  val ages: Map[Int,Long] =
    input
      .split(",")
      .groupMapReduce(_.toInt)(_ => 1L)(_ + _)

  val FirstSpawnAfter = 8
  val SpawnAfter      = 6
  val SpawnNow        = -1

  extension (generation: Map[Int,Long])

    def next: Map[Int,Long] =
      val aged    = generation.map((timer,count) => (timer - 1) -> count)
      val spawned = aged.getOrElse(SpawnNow, 0L)
      val next    = aged.getOrElse(SpawnAfter, 0L) + spawned
      aged.removed(SpawnNow) + (FirstSpawnAfter -> spawned) + (SpawnAfter -> next)


  def solve(generations: Map[Int,Long], years: Int): Long =
    Iterator.iterate(generations)(_.next).nth(years).values.sum

  lazy val answer1: Long = solve(ages, 80)
  lazy val answer2: Long = solve(ages, 256)
