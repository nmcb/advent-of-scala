package aoc2022

import nmcb.*

import scala.io.*
import scala.math.*

object Day15 extends AoC:

  case class Pos(x: Int, y: Int)

  case class Cover(min: Int, max: Int)

  case class Lock(s: Pos, b: Pos):

    val manhatten: Int =
      abs(s.x - b.x) + abs(s.y - b.y)

    def cover(y: Int): Option[Cover] =
      if y >= s.y - manhatten && y <= s.y + manhatten then
        Some(Cover(s.x + abs(y - s.y) - manhatten, s.x - abs(y - s.y) + manhatten))
      else
        None

  val locks: List[Lock] =
    Source
      .fromResource(s"$day.txt")
      .getLines()
      .toList
      .map {
        case s"Sensor at x=$sx, y=$sy: closest beacon is at x=$bx, y=$by" =>
          Lock(Pos(sx.toInt,sy.toInt), Pos(bx.toInt,by.toInt))
      }

  lazy val answer1: Int =
    val covers: List[Cover] = locks.flatMap(_.cover(2000000))
    val maxX: Int = max(covers.map(_.max).max, 2000000)
    val minX: Int = min(covers.map(_.min).min, 0)
    (minX to maxX).foldLeft(0): (count,x) =>
      if covers.exists(c => c.min <= x && c.max >= x) then count + 1 else count

  lazy val answer2: Long =
    var answer: Long = 0
    for
      y <- 0 to 4000000
    yield
      val lcs = locks.flatMap(_.cover(y)).sortBy(_.min)
      lcs.foldLeft(0): (x,cur) =>
          if cur.min > x then answer = x.toLong * 4000000 + y
          max(x + 1, cur.max)
    answer
