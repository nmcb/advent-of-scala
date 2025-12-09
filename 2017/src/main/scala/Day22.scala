import Day22.Status.Clean

import nmcb.predef.*
import scala.io.Source

object Day22 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  enum Dir:
    case N
    case S
    case E
    case W

    def turnLeft: Dir =
      this match
        case N => W
        case W => S
        case S => E
        case E => N

    def turnRight: Dir =
      this match
        case N => E
        case E => S
        case S => W
        case W => N

    def reverse: Dir =
      this match
        case N => S
        case S => N
        case E => W
        case W => E

  import Dir.*

  case class Pos(x: Int, y: Int):

    def step(dir: Dir): Pos =
      dir match
        case N => copy(y = y - 1)
        case S => copy(y = y + 1)
        case E => copy(x = x + 1)
        case W => copy(x = x - 1)

  enum Status:
    case Clean, Infected, Weakened, Flagged

  import Status.*

  case class Carrier(nodes: Map[Pos,Status], current: Pos, dir: Dir, infected: Int = 0):

    def wake1: Carrier =
      nodes(current) match
        case Clean =>
          val turn = dir.turnLeft
          copy(
            nodes    = nodes + (current -> Infected),
            current  = current.step(turn),
            dir      = turn,
            infected = infected + 1
          )
        case Infected =>
          val turn = dir.turnRight
          copy(
            nodes   = nodes + (current -> Clean),
            current = current.step(turn),
            dir     = turn
          )
        case _ =>
          sys.error(s"invalid state: $current -> ${nodes(current)}")

    def wake2: Carrier =
      nodes(current) match
        case Clean =>
          val turn = dir.turnLeft
          copy(
            nodes   = nodes + (current -> Weakened),
            current = current.step(turn),
            dir     = turn
          )
        case Weakened =>
          copy(
            nodes    = nodes + (current -> Infected),
            current = current.step(dir),
            infected = infected + 1
          )
        case Infected =>
          val turn = dir.turnRight
          copy(
            nodes   = nodes + (current -> Flagged),
            current = current.step(turn),
            dir     = turn
          )
        case Flagged =>
          val turn = dir.reverse
          copy(
            nodes   = nodes + (current -> Clean),
            current = current.step(turn),
            dir     = turn
          )

  private val carrier: Carrier =

    val lines: Vector[String] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector

    val current: Pos =
      Pos(lines(0).length / 2, lines.length / 2)

    val nodes: Map[Pos,Status] =
      lines
        .zipWithIndex
        .flatMap: (row,y) =>
          row
            .zipWithIndex
            .map:
              case ('#',x) => Pos(x,y) -> Infected
              case ('.',x) => Pos(x,y) -> Clean
              case ( c ,_) => sys.error(s"unexpected char=$c")
        .toMap
        .withDefaultValue(Clean)

    Carrier(nodes, current, Dir.N)

  def solve(init: Carrier, iterations: Int)(next: Carrier => Carrier): Int =
    Iterator.iterate(init)(next).nth(iterations).infected

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = solve(carrier, 10000)(_.wake1)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = solve(carrier, 10000000)(_.wake2)
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
