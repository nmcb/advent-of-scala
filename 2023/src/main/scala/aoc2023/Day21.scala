package aoc2023

import nmcb.*

import scala.annotation.tailrec

/** @see Credits for part 2 - https://github.com/merlinorg */
object Day21 extends AoC:

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

    def rock(p: Pos): Boolean =
      get(p).exists(_ == '#')

    def plot(p: Pos): Boolean =
      get(p).exists(c => c == '.' || c == 'S')

    /** part 1 - simple bfs */

    def solve1(steps: Int): Int =
      @tailrec
      def loop(count: Int, found: Set[Pos] = Set(startPos)): Set[Pos] =
        if count == 0 then found else loop(count - 1, found.flatMap(_.neighbours.filter(plot)))
      loop(steps).size


    /** part 2 - utilises the input grid being square and the solution scaling quadratically in grid size */

    assert(sizeX == sizeY)
    val gridSize: Int = sizeY

    /** provides for an infinite garden using this grid as tiles */
    def infinite(p: Pos): Char =
      val wrapX: Int = (p.x % sizeX + sizeX) % sizeX
      val wrapY: Int = (p.y % sizeY + sizeY) % sizeY
      grid(wrapY)(wrapX)

    final case class Grid(steps: Int = 0, plots: Set[Pos] = Set(startPos)):
      def next: Grid =
        copy(steps + 1,
          for
            plot <- plots
            step <- plot.neighbours
            if infinite(step) != '#'
          yield step)

    case class Collect(steps: Long, gridPlotsScan: Vector[Long] = Vector.empty):
      val stepsToGrids = steps / gridSize
      val gridsToSteps = steps % gridSize

      infix def add(grid: Grid): Collect =
        if grid.steps % gridSize == gridsToSteps then
          copy(gridPlotsScan = gridPlotsScan :+ grid.plots.size)
        else
          this

      /** given three distinct subsequent `y` points on a quadratic polynomial, solve `y`` for given `x` */
      def quadratic(y0: Long, y1: Long, y2: Long)(x: Long): Long =
        y0 + (y1 - y0) * x + (x * (x - 1) / 2) * (y2 - 2 * y1 + y0)

      def solution: Option[Long] =
        gridPlotsScan match
          case Vector(plots0, plots1, plots2) => Some(quadratic(plots0, plots1, plots2)(stepsToGrids))
          case _                              => None

    def solve2(steps: Int): Long =
      Iterator
        .iterate(Grid())(_.next)
        .scanLeft(Collect(steps))(_ add _)
        .flatMap(_.solution)
        .next

  val garden: Garden = Garden(lines.map(_.toVector))


  lazy val answer1: Int = garden.solve1(64)
  lazy val answer2: Long = garden.solve2(26501365)
