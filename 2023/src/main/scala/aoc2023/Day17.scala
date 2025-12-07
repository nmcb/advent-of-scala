package aoc2023

import nmcb.*

import scala.collection.mutable

object Day17 extends AoC:

  case class Pos(x: Int, y: Int):
    def unary_- =
      Pos(-1 * x, -1 * y)

    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

  object Pos:
    val zero: Pos =
      Pos(0, 0)

    val directions: List[Pos] =
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

  case class Crucible(current: Pos, direction: Pos, steps: Int):
    def canMove1(dir: Pos): Boolean =
      steps < 3 || dir != direction

    def canStop1: Boolean =
      true

    def canMove2(dir: Pos): Boolean =
      (dir == direction && steps < 10) || (dir != direction && steps >= 4) || direction == Pos.zero

    def canStop2: Boolean =
      steps >= 4


  case class City(grid: Grid[Int]):
    def leastHeatLoss(canMove: Crucible => Pos => Boolean, canStop: Crucible => Boolean): Option[Int] =

      def reachable(crucible: Crucible): List[(Crucible, Int)] =
        for
          direction <- Pos.directions.filterNot(_ == -crucible.direction)
          if canMove(crucible)(direction)
          next = crucible.current + direction
          if grid.peek(next).isDefined
          steps = if direction == crucible.direction then crucible.steps + 1 else 1
        yield
          Crucible(next, direction, steps) -> grid(next)

      val start = Crucible(Pos.zero, Pos.zero, 0)
      val target = (n: Crucible) => n.current == Pos(grid.sizeX - 1, grid.sizeY - 1) && canStop(n)

      Dijkstra.traverse[Crucible](start, target, reachable).map((_, loss) => loss)

  object Dijkstra:

    /**
     * Specialised version of Dijkstra's algorithm that provides for callbacks deciding whether a target has been
     * reached, and that allows for node weight deltas to be added when reachable edge weights are (re-)computed.
     * @param start A node to start searching from.
     * @param target A node callback returning whether given node was a target node.
     * @param reachable A callback returning reachable nodes and their weight deltas from given node.
     * @tparam A The type of node.
     * @return The target node and associated traversal weight if reachable.
     */
    def traverse[A](start: A, target: A => Boolean, reachable: A => List[(A, Int)]): Option[(A, Int)] =
      val todo    = mutable.PriorityQueue.empty(using Ordering.Int.on[(Int, A)](_._1).reverse)
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

  lazy val city: City = City(lines.map(_.map(_.asDigit).toVector))


  lazy val answer1: Int = city.leastHeatLoss(_.canMove1, _.canStop1).get
  lazy val answer2: Int = city.leastHeatLoss(_.canMove2, _.canStop2).get
