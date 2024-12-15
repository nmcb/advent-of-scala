import scala.annotation.*
import scala.io.*

object Day15 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Dir = Char

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Pos =
      d match
        case '^' => copy(y = y - 1)
        case '>' => copy(x = x + 1)
        case 'v' => copy(y = y + 1)
        case '<' => copy(x = x - 1)

    def coordinate: Int =
      100 * y + x


  case class Grid(grid: Map[Pos,Char]):
    val sizeX: Int = grid.keys.maxBy(_.x).x + 1
    val sizeY: Int = grid.keys.maxBy(_.y).y + 1
    val walls: Set[Pos] = grid.filter((_,c) => c == '#').keySet
    val boxes: Set[Pos] = grid.filter((_,c) => c == 'O').keySet
    val robot: Pos      = grid.find((_,c) => c == '@').map(_._1).head

    def within(p: Pos): Boolean =
      p.x >= 0 & p.y >= 0 & p.x < sizeX & p.y < sizeY

    override def toString: String =
      val representation = for {
        y <- 0 until sizeY
        x <- 0 until sizeX
        p = Pos(x, y)
      } yield grid(p)
      representation.mkString("").grouped(sizeX).mkString("\n")

    def moves(p: Pos, d: Dir): List[(Pos,Pos)] =
      def loop(current: Pos, result: List[(Pos,Pos)] = List.empty): List[(Pos,Pos)] =
        val n = current.move(d)
        grid(n) match
          case '.' => (current -> n) :: result
          case '#' => List.empty
          case 'O' => loop(n, (current -> n) :: result)
      loop(p)

    infix def push(d: Dir): Grid =
      val updates = moves(robot, d)
      if updates.isEmpty then
        this
      else
        val next =
          updates
            .foldLeft(grid):
              case (g,(f,t)) => g.updated(t, grid(f))
            .updated(robot, '.')
        Grid(next)



  val (grid: Grid, moves: List[Dir]) =
    val Array(top, bottom) = Source.fromResource(s"input$day.txt").mkString.split("\n\n").map(_.trim)
    val positions = for {
      (l, y) <- top.split("\n").zipWithIndex
      (c, x) <- l.trim.zipWithIndex
    } yield Pos(x, y) -> c
    val moves = bottom.filterNot(_ == '\n').toList
    (Grid(positions.toMap), moves)

  val start1: Long  = System.currentTimeMillis
  val answer1: Int = moves.foldLeft(grid)(_ push _).boxes.map(_.coordinate).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


