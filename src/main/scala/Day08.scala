import scala.io.*
import nmcb.*
import predef.*

import Grid.*

object Day08 extends App:
  val day: String = getClass.getName.drop(3).init

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


  val grid: Grid[Char] = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grid.pairs.flatMap((c,ps) => ps.flatMap(grid.createTwice)).size
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = grid.pairs.flatMap((c,ps) => ps.flatMap(grid.createInline)).size
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
