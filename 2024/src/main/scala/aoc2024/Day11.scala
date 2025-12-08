package aoc2024

import nmcb.*
import scala.annotation.*

object Day11 extends AoC:

  type Stone = String

  extension (s: Stone) def dropLeadingZeros: Stone =
    val dropped = s.dropWhile(_ == '0')
    if dropped.isEmpty then "0" else dropped

  type StoneCount  = (stone: Stone, count: Long)

  def update(ss: StoneCount): Vector[StoneCount] =
    val handle = ss.stone match
      case "0" =>
        Vector("1")
      case s if s.length % 2 == 0 =>
        val (l, r) = s.splitAt(s.length / 2)
        Vector(l.dropLeadingZeros, r.dropLeadingZeros)
      case n =>
        Vector((n.toLong * 2024).toString)

    handle.map(_ -> ss.count) :+ (ss.stone -> -ss.count)

  val stones: Vector[StoneCount] = input.split(' ').toVector.map(_ -> 1L)

  def blink(n: Int, stones: Vector[StoneCount]): Long =
    @tailrec
    def loop(result: Vector[StoneCount], blinked: Int = 0): Long =
      if blinked >= n then
        result.map(_.count).sum
      else
        loop(
          result
            .foldLeft(Vector.empty[StoneCount]): (updates, ss: StoneCount) =>
              updates ++ update(ss)
            .groupMapReduce(_.stone)(_.count)(_ + _)
            .foldLeft(result.map(_.toTuple).toMap): (next, ss: StoneCount) =>
                next.updatedWith(ss.stone):
                  case Some(count) if count == (-ss.count) => None
                  case Some(count)                         => Some(count + ss.count)
                  case None                                => Some(ss.count)
            .toVector,
          blinked + 1
        )
    loop(stones)

  lazy val answer1: Long = blink(25, stones)
  lazy val answer2: Long = blink(75, stones)
