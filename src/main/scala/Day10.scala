import scala.io.*

object Day10 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  enum Dir:
    case N, E, S, W

  import Dir.*

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Pos =
      d match
        case N => Pos(x, y - 1)
        case E => Pos(x + 1, y)
        case S => Pos(x, y + 1)
        case W => Pos(x - 1, y)

  case class Grid(grid: Vector[Vector[Int]]):
    val sizeX = grid.head.size
    val sizeY = grid.size

    def within(p: Pos): Boolean =
      p.x >= 0 & p.y >= 0 & p.x < sizeX & p.y < sizeY

    def peek(p: Pos): Int =
      if within(p) then grid(p.y)(p.x) else sys.error(s"out of bounds: $p")

    def neighbours(p: Pos): Set[Pos] =
      Set(N, E, S, W).map(p.move).filter(within)

    lazy val positions: Set[Pos] =
      for {
        y <- (0 until sizeY).toSet
        x <- (0 until sizeX).toSet
      } yield Pos(x, y)

    lazy val starts: Set[Pos] =
      positions.filter(p => peek(p) == 0)

    def paths(start: Pos): Set[Vector[Pos]] =

      def step(trail: Vector[Pos]): Set[Vector[Pos]] =
        neighbours(trail.last).filter(n => within(n) & peek(n) == peek(trail.last) + 1).map(p => trail :+ p)

      def loop(trails: Set[Vector[Pos]], current: Int): Set[Vector[Pos]] =
        val result = trails.filter(t => peek(t.last) == current)
        if current >= 9 then result else loop(result.flatMap(step), current + 1)

      loop(Set(Vector(start)), 0)

    def solve1: Int =
      starts.flatMap(paths).groupMap(_.head)(_.last).values.map(_.size).sum

    def solve2: Int =
      starts.flatMap(paths).size

  val grid: Grid =
    Grid(Source.fromResource(s"input$day.txt").getLines.map(_.map(_.asDigit).toVector).toVector)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grid.solve1
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = grid.solve2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
