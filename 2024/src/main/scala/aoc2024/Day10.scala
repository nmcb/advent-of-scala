package aoc2024

import nmcb.*
import pos.*
import nmcb.predef.*

object Day10 extends AoC:

  val grid: Grid[Int] = Grid.fromLines(lines).map(_.asDigit)

  val heads: Set[Pos] =
    grid.filter(_.element == 0).map(_.pos)

  type TrailHead = Set[Vector[Pos]]

  extension (trails: TrailHead)
    def reachableSummits: Iterable[Int] =
      trails.groupMap(_.head)(_.last).values.map(_.size)


  extension (g: Grid[Int])

    def trailsFrom(head: Pos): TrailHead =

      def step(trail: Vector[Pos]): TrailHead =
        trail.last.adjoint
          .filter(n => g.within(n) && g.peek(n) == g.peek(trail.last) + 1)
          .map(p => trail :+ p)

      def loop(trails: TrailHead, current: Int): TrailHead =
        val result = trails.filter(trail => g.peek(trail.last) == current)
        if current >= 9 then result else loop(result.flatMap(step), current + 1)

      loop(Set(Vector(head)), 0)

    def score: Long =
      heads.flatMap(grid.trailsFrom).reachableSummits.sum

    def rating: Long =
      heads.flatMap(grid.trailsFrom).size


  lazy val answer1: Long = grid.score
  lazy val answer2: Long = grid.rating
