import nmcb.*
import predef.*
import scala.io.*
import scala.collection.*

import Dir.*

object Day21 extends App:

  val day: String           = getClass.getName.filter(_.isDigit).mkString("")
  val input: Vector[String] = Source.fromResource(s"input$day.txt").getLines.toVector

  val numGrid: Grid[Char] =
    Grid.fromString(
      """789
        |456
        |123
        |.0A
        |""".stripMargin)

  val dirGrid: Grid[Char] =
    Grid.fromString(
      """.^A
        |<v>
        |""".stripMargin)

  val buttonBy: Map[Dir,Char] =
    Map(
      E -> '>',
      W -> '<',
      N -> '^',
      S -> 'v'
    )

  def solve(codes: Vector[String], robots: Int): Long =
    val cache = memo[(Pos,Pos,Int),Long]()

    def gridBy(robot: Int): Grid[Char] =
      if robot == 0 then numGrid else dirGrid

    def solution(code: Vector[Char], robot: Int): Long =
      code
        .foldLeft[(Pos,Long)](gridBy(robot).findOne('A') -> 0L): (counts,char) =>
          val to = gridBy(robot).findOne(char)
          to -> (counts.element + shortest(counts.pos, to, robot))
        .element

    def shortest(from: Pos, to: Pos, robot: Int): Long = cache.memoize(from,to,robot):
      Dijkstra
        .breadthFirstSearch((from, Vector.empty[Char])):
          case (p, code) if p == to => Right(
              if robot < robots then
                solution(code :+ 'A', robot + 1)
              else
                code.length + 1L
            )
          case (p, code) => Left(
              p.pathToAdj(to)
                .filterNot(d => gridBy(robot).contains(p + d, '.'))
                .map(d => (p + d, code :+ buttonBy(d)))
                .toSet
            )
        .min

    codes.map(code => solution(code.toVector, 0) * code.filter(_.isDigit).toLong).sum

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve(input, 2)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve(input, 25)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
