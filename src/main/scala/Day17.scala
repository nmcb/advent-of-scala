import scala.annotation.tailrec
import scala.io.*
import scala.collection.mutable

object Day17 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):
    def unary_- =
      Pos(-1 * x, -1 * y)
    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

  object Pos:
    val zero: Pos =
      Pos(0, 0)

    val offsets: List[Pos] =
      List(Pos(0, 1), Pos(0, -1), Pos(1, 0), Pos(-1, 0))

  type Grid[A] = Vector[Vector[A]]

  object Grid:
    extension [A](g: Grid[A]) def sizeX: Int =
      g.map(_.size).max

    extension[A] (g: Grid[A]) def sizeY: Int =
      g.size

    extension[A] (g: Grid[A]) def peek(p: Pos): Option[A] =
      g.lift(p.y).flatMap(_.lift(p.x))

    extension[A] (g: Grid[A]) def apply(p: Pos): A =
      g.lift(p.y).flatMap(_.lift(p.x)).getOrElse(sys.error(s"out of bounds: p=$p"))

  import Grid.*

  case class Node(pos: Pos, dir: Pos, steps: Int):
    def canMove1(offset: Pos): Boolean =
      steps < 3 || offset != dir

    def canStop1: Boolean =
      true

    def canMove2(offset: Pos): Boolean =
      dir == Pos.zero || (offset == dir && steps < 10) || (offset != dir && steps >= 4)

    def canStop2: Boolean =
      steps >= 4


  case class City(grid: Grid[Int]):

    def leastHeatLoss(canMove: Node => Pos => Boolean, canStop: Node => Boolean): Option[Int] =

      def reachable(n: Node): List[(Node, Int)] =
        for
          offset <- Pos.offsets
          if offset != -n.dir && canMove(n)(offset)
          next = n.pos + offset
          if grid.peek(next).isDefined
          steps = if offset == n.dir then n.steps + 1 else 1
        yield
          Node(next, offset, steps) -> grid(next)

      val start = Node(Pos.zero, Pos.zero, 0)
      val target = (n: Node) => n.pos == Pos(grid.sizeX - 1, grid.sizeY - 1) && canStop(n)

      Dijkstra.traverse(start, target, reachable).map((_, loss) => loss)

  object Dijkstra:

    /**
     * Specialised version of Dijkstra's algorithm that provides for callbacks deciding whether a target has been
     * reached, and that allows for node weight deltas to be added when reachable nodes are computed.
     * @param start A node to start searching from.
     * @param target A node callback returning whether given node was the target node.
     * @param reachable A callback returning reachable nodes with weight deltas from given node.
     * @tparam A The type of node.
     * @return The target node and associated traversal weight if reachable.
     */
    def traverse[A](start: A, target: A => Boolean, reachable: A => List[(A, Int)]): Option[(A, Int)] =
      val todo    = mutable.PriorityQueue.empty[(Int, A)](Ordering.by((d,_) => -d))
      val weights = mutable.Map.empty[A, Int]

      def enqueue(node: A, weight: Int): Unit =
        if !weights.contains(node) then todo.enqueue((weight, node))

      enqueue(start, 0)

      while (todo.nonEmpty)
        val (weight, node) = todo.dequeue
        if !weights.contains(node) then
          weights(node) = weight
          if target(node) then return Some(node -> weight)
          reachable(node).foreach((reach, delta) => enqueue(reach, weight + delta))
      None

  lazy val city: City =
    City(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(_.map(_.asDigit).toVector)
        .toVector
    )

  val start1: Long = System.currentTimeMillis
  val answer1: Int = city.leastHeatLoss(_.canMove1, _.canStop1).get
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long = System.currentTimeMillis
  val answer2: Int = city.leastHeatLoss(_.canMove2, _.canStop2).get
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

