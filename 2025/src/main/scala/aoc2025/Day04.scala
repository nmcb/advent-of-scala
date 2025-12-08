package aoc2025

import nmcb.*

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object Day04 extends AoC:

  type Pos = (x: Int, y: Int)

  given Ordering[Pos] = Ordering.by(_.toTuple)

  val (minPos, maxPos, rolls) =
    val ps = for y <- lines.indices.toSet ; x <- lines.head.indices.toSet yield (x = x, y = y)
    (ps.min, ps.max, ps.filter(p => lines(p.y)(p.x) == '@'))

  extension (p: Pos)

    infix def +(that: Pos): Pos =
      (p.x + that.x, p.y + that.y)

    def neighbours: Set[Pos] =
      Set((-1,-1), (0,-1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
        .map(_ + p)
        .filter(p => p >= minPos && p <= maxPos)

  extension (rolls: Set[Pos])

    def neighbouring(p: Pos): Set[Pos] =
      p.neighbours.intersect(rolls)

    def accessible: Set[Pos] =
      rolls.filter(p => rolls.neighbouring(p).size < 4)

    @tailrec
    def clearAll: Set[Pos] =
      val remove = accessible
      if remove.isEmpty then rolls else (rolls -- remove).clearAll

  lazy val answer1: Int = rolls.accessible.size
  lazy val answer2: Int = (rolls -- rolls.clearAll).size
