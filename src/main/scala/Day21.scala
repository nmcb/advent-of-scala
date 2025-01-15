import nmcb.*
import predef.*
import scala.io.*
import scala.collection.*

import Dir.*

object Day21 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  type Button = Char
  type Pushes = Vector[Button]

  object Button:
    val enter: Button = 'A'
    val empty: Button = '.'

  val numericKeypad: Grid[Button] =
    Grid.fromString(
      """789
        |456
        |123
        |.0A
        |""".stripMargin)

  val directionKeypad: Grid[Button] =
    Grid.fromString(
      """.^A
        |<v>
        |""".stripMargin)

  val directionButtonBy: Map[Dir,Button] =
    Map(
      E -> '>',
      W -> '<',
      N -> '^',
      S -> 'v'
    )

  extension (t: (Pos,Long))
    def pos: Pos    = t._1
    def count: Long = t._2

  def solve(buttonPushes: Vector[Pushes], robots: Int): Long =
    val cache = memo[(Pos,Pos,Int),Long]()

    def keypadBy(robot: Int): Grid[Button] =
      if robot == 0 then numericKeypad else directionKeypad

    def pointerMovesFor(outputs: Vector[Button], robot: Int): Long =
      val start = keypadBy(robot).findOne(Button.enter)
      outputs
        .foldLeft[(Pos,Long)](start -> 0L):
          case (moves, button) =>
            val to = keypadBy(robot).findOne(button)
            to -> (moves.count + shortestPathMoves(moves.pos, to, robot))
        .count

    def shortestPathMoves(from: Pos, to: Pos, robot: Int): Long =
      cache.memoize(from,to,robot):
        Dijkstra
          .breadthFirstSearch((from,Vector.empty[Button])):
            case (p,pushes) if p == to => Right(
                if robot < robots then
                  pointerMovesFor(pushes :+ Button.enter, robot + 1)
                else
                  println(s"from=$from, to=$to, length=${pushes.length + 1L}, manhattan=${from manhattan to}")
                  pushes.length + 1L
              )
            case (p,code) => Left(
                p.directionTo(to)
                  .filterNot(d => keypadBy(robot).contains(p step d, Button.empty))
                  .map(d => (p step d, code :+ directionButtonBy(d)))
                  .toSet
              )
          .min

    buttonPushes
      .map: pushes =>
        pointerMovesFor(pushes, 0) * pushes.filter(_.isDigit).mkString("").toLong
      .sum

  val input: Grid[Char] = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve(input.matrix, 2)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve(input.matrix, 25)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
