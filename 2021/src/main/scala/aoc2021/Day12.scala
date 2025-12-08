package aoc2021

import nmcb.*

object Day12 extends AoC:

  type Cave  = (String, List[String])
  type Caves = Map[String, List[String]]

  val caves: Caves =
    lines
      .foldLeft[Caves](Map().withDefaultValue(Nil)): (cave, line) =>
        val Array(start,end) = line.split("-")
        cave.updated(start, end :: cave(start)).updated(end, start :: cave(end))

  def decend(caves: Caves)(traverse: Cave => Boolean): Int =

    def isSmall(name: String): Boolean =
      name.head.isLower

    def loop(current: String, path: List[String]): Int =
      caves(current).foldLeft(0): (total,next) =>
        if next == "end" then
          total + 1
        else if next == "start" || (isSmall(next) && traverse(next, path.filter(isSmall))) then
          total
        else
          total + loop(next, next :: path)

    loop("start", Nil)

  def solve2(caves: Caves): Int =
    decend(caves): (next, path) =>
      val occurrences = (next :: path).groupBy(identity).values.map(_.length)
      occurrences.exists(_ > 2) || occurrences.count(_ == 2) > 1

  lazy val answer1: Int = decend(caves)((next, path) => path.contains(next))
  lazy val answer2: Int = solve2(caves)
