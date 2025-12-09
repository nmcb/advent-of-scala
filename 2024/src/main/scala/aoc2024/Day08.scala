package aoc2024

import nmcb.*
import pos.*
import nmcb.predef.*

object Day08 extends AoC:

  extension (ps: Set[Pos]) def pairAll: Set[(Pos, Pos)] =
    ps.toSeq.combinations(2).map(p => p(0) -> p(1)).toSet


  extension (g: Grid[Char])
    def pairs: Set[(Char,Set[(Pos,Pos)])] =
      g.elements
        .filter(_.element != '.')
        .groupMap(_.element)(_.pos)
        .map((c,ps) => c -> ps.pairAll)
        .toSet

    def createTwice(a: Pos, b: Pos): Set[Pos] =
      Set(a + a - b, b + b - a).filter(g.within)

    def createInline(a: Pos, b: Pos): Set[Pos] =
      def loop(todo: Set[(Pos,Pos)], result: Set[Pos] = Set.empty): Set[Pos] =
        val found = todo.flatMap(createTwice) ++ result + a + b
        if found.size == result.size then result else loop(found.pairAll, found)
      loop(Set((a, b)))


  val grid: Grid[Char] = Grid.fromLines(lines)

  lazy val answer1: Long = grid.pairs.flatMap((c,ps) => ps.flatMap(grid.createTwice)).size
  lazy val answer2: Long = grid.pairs.flatMap((c,ps) => ps.flatMap(grid.createInline)).size
