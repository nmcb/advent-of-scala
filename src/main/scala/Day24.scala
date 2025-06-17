import scala.annotation.tailrec
import scala.io.Source

object Day24 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):

    infix def +(that: Pos): Pos =
      copy(x = x + that.x, y = y + that.y)

    def neighbours: Set[Pos] =
      Set(Pos(-1, 0), Pos(1, 0), Pos(0, -1), Pos(0, 1)).map(_ + this)

  type Grid      = Set[Pos]
  type Nodes     = Map[Int,Pos]
  type Distances = Map[Int,Map[Int,Int]]

  val(grid: Grid, nodes: Nodes) =
    val lines = Source.fromResource(s"input$day.txt").getLines.toVector
    val grid  = for y <- lines.indices ; x <- lines.head.indices if lines(y)(x) != '#'  yield Pos(x, y)
    val nodes = for y <- lines.indices ; x <- lines.head.indices if lines(y)(x).isDigit yield lines(y)(x).asDigit -> Pos(x, y)
    (grid.toSet, nodes.toMap)

  /** breath first search */
  def distance(grid: Grid, start: Pos, end: Pos): Int =
    import scala.collection.*
    val steps = mutable.Map(start -> 0)
    val todo  = mutable.Queue(start)

    while todo.nonEmpty do
      val current = todo.dequeue
      if current == end then return steps(current)
      val update  = steps(current) + 1
      current.neighbours.intersect(grid).foreach: next =>
        if !steps.contains(next) || update < steps(next) then
          steps(next) = update
          todo.enqueue(next)
    sys.error("boom!")

  def distances(grid: Grid, nodes: Nodes): Distances =
    nodes.transform: (_, start) =>
      nodes.transform: (_, end) =>
        distance(grid, start, end)

  /** brute force traveling salesman */

  type Routes = Vector[Vector[Int]]

  def shortestPath(distances: Distances, routes: Routes): Int =
    routes
      .map: route =>
        route.sliding(2).map: step =>
          distances(step.head)(step.last)
        .sum
      .min

  def solve1(grid: Grid, nodes: Nodes): Int =
    val graph  = distances(grid, nodes)
    val routes = (1 to nodes.keys.max).toVector.permutations.map(0 +: _).toVector
    shortestPath(graph, routes)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(grid, nodes)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(grid: Grid, nodes: Nodes): Int =
    val graph  = distances(grid, nodes)
    val routes = (1 to nodes.keys.max).toVector.permutations.map(0 +: _ :+ 0).toVector
    shortestPath(graph, routes)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(grid, nodes)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
