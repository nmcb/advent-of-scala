import scala.annotation.tailrec
import scala.io.Source

object Day15 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    infix def +(that: Pos): Pos =
      copy(x = x + that.x, y = y + that.y)

  type Grid = Map[Pos, Int]

  val directions = Vector(Pos(1, 0), Pos(-1, 0), Pos(0, -1), Pos(0, 1))

  extension (grid: Grid)
    def neighbours(pos: Pos): Vector[Pos] =
      directions.map(_ + pos).filter(grid.contains)

  val grid: Grid =

    val input =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector

    Vector
      .tabulate(input(0).length, input.length): (x,y) =>
        Pos(x, y) -> input(y)(x).asDigit
      .flatten
      .toMap


  def solve(grid: Grid): Int =

    val start = Pos(0,0)
    val end   = grid.keys.maxBy(p => p.x * p.y)

    @tailrec
    def dijkstra(todo: Set[Pos], risk: Grid): Int =
      val current = todo.minBy(risk)
      if current == end then
        risk(end)
      else
        val (nextTodo, nextRisk) =
          grid
            .neighbours(current)
            .filter(n => !risk.contains(n) || risk(current) + grid(n) < risk(n))
            .foldLeft((todo - current, risk)):
              case ((todo, risk), next) =>
                (todo + next, risk.updated(next, risk(current) + grid(next)))

        dijkstra(nextTodo, nextRisk)

    dijkstra(Set(start), Map(start -> 0))

  val start1  = System.currentTimeMillis
  lazy val answer1 = solve(grid)
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def expanded(grid: Grid): Grid =
    val end   = grid.keys.maxBy(p => p.x * p.y)
    val sizeX = end.x + 1
    val sizeY = end.y + 1
    Vector
      .tabulate(5,5): (nx,ny) =>
        grid.toVector.map: (p,v) =>
          val x = nx * sizeX + p.x
          val y = ny * sizeY + p.y
          val risk = 1 + (v - 1 + nx + ny) % 9
          Pos(x,y) -> risk
      .flatten
      .flatten
      .toMap

  val start2  = System.currentTimeMillis
  lazy val answer2 = solve(expanded(grid))
  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
