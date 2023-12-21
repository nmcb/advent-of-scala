import scala.annotation.tailrec
import scala.io.Source

/** credits for part 2 go to https://github.com/merlinorg */
object Day21 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):

    def +(b: Pos): Pos =
      Pos(x + b.x, y + b.y)

    def neighbours: Set[Pos] =
      Set(Pos(1, 0), Pos(-1, 0), Pos(0, 1), Pos(0, -1)).map(_ + this)


  case class Garden(grid: Vector[Vector[Char]]):
    val sizeX: Int = grid.head.size
    val sizeY: Int = grid.size

    val positions: Set[Pos] =
      val result =
        for
          y <- 0 until sizeY
          x <- 0 until sizeX
        yield
          Pos(x, y)
      result.toSet

    lazy val startPos: Pos =
      positions
        .find(p => get(p) contains 'S')
        .getOrElse(sys.error("no start position"))

    def get(p: Pos): Option[Char] =
      grid.lift(p.y).flatMap(_.lift(p.x))

    def wrap(p: Pos): Char =
      val wrapX: Int = (p.x % sizeX + sizeX) % sizeX
      val wrapY: Int = (p.y % sizeY + sizeY) % sizeY
      grid(wrapY)(wrapX)

    def rock(p: Pos): Boolean =
      get(p).exists(_ == '#')

    def plot(p: Pos): Boolean =
      get(p).exists(c => c == '.' || c == 'S')

    /** part 1 - simple bfs */

    def solve1(steps: Int): Int =
      def loop(count: Int, found: Set[Pos] = Set(startPos)): Set[Pos] =
        if count == 0 then found else loop(count - 1, found.flatMap(_.neighbours.filter(plot)))
      loop(steps).size

    /** part 2 - utilises the input grid being square and scaling quadratically in grid size */

    assert(sizeX == sizeY)
    val gridSize: Int = sizeY

    final case class Search(index: Int = 0, positions: Set[Pos] = Set(startPos)):
      def next: Search =
        copy(index + 1,
          for
            p <- positions
            n <- p.neighbours
            if wrap(n) != '#'
          yield n)

    case class Collect(steps: Long, found: Vector[Long] = Vector.empty):
      val div = steps / gridSize
      val mod = steps % gridSize

      def add(search: Search): Collect =
        if search.index % gridSize == mod then
          copy(found = found :+ search.positions.size)
        else
          this

      /** given three `y` points solve for `x` */
      def quadratic(y0: Long, y1: Long, y2: Long)(x: Long): Long =
        y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

      def solution: Option[Long] =
        found match
          case Vector(y0, y1, y2) => Some(quadratic(y0, y1, y2)(div))
          case _                  => None

    def solve2(steps: Int): Long =
      Iterator
        .iterate(Search())(_.next)
        .scanLeft(Collect(steps))(_ add _)
        .flatMap(_.solution)
        .next

  val garden: Garden =
    Garden(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(_.toVector)
        .toVector
    )

  val start1: Long = System.currentTimeMillis
  val answer1: Int = garden.solve1(64)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = garden.solve2(26501365)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
