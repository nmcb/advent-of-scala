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

  /** clean nodes are removed and modeled via default value '.' */
  case class Carrier(nodes: Map[Pos,Char], current: Pos, dir: Dir, infected: Int = 0):

    def wake1: Carrier =
      nodes(current) match
        case '.' =>
          val turn = dir.turnLeft
          copy(
            nodes    = nodes + (current -> '#'),
            current  = current.step(turn),
            dir      = turn,
            infected = infected + 1
          )
        case '#' =>
          val turn = dir.turnRight
          copy(
            nodes   = nodes - current,
            current = current.step(turn),
            dir     = turn
          )

    def wake2: Carrier =
      nodes(current) match
        case '.' =>
          val turn = dir.turnLeft
          copy(
            nodes   = nodes + (current -> 'W'),
            current = current.step(turn),
            dir     = turn
          )
        case 'W' =>
          copy(
            nodes    = nodes + (current -> '#'),
            current = current.step(dir),
            infected = infected + 1
          )
        case '#' =>
          val turn = dir.turnRight
          copy(
            nodes   = nodes + (current -> 'F'),
            current = current.step(turn),
            dir     = turn
          )
        case 'F' =>
          val turn = dir.reverse
          copy(
            nodes   = nodes - current,
            current = current.step(turn),
            dir     = turn
          )

  private val carrier: Carrier =

    val lines =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector

    val current: Pos =
      Pos(lines(0).size / 2, lines.size / 2)

    val nodes: Map[Pos,Char] =
      lines
        .zipWithIndex
        .flatMap: (row, y) =>
          row
            .zipWithIndex
            .flatMap:
              case ('#', x) => Some(Pos(x,y) -> '#')
              case ('.', _) => None
        .toMap
        .withDefaultValue('.')

    Carrier(nodes, current, Dir.N)

  def solve(init: Carrier, iterations: Int)(next: Carrier => Carrier): Int =
    Iterator.iterate(carrier)(next).drop(iterations).next.infected

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve(carrier, 10000)(_.wake1)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(carrier, 10000000)(_.wake2)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
