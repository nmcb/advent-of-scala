package nmcb

import nmcb.extensions.*
import scala.annotation.*

final case class Edge[A](from: A, to: A, distance: Int):
  def inverse: Edge[A] = Edge(to, from, distance)

final case class Graph[A](neighbours: Map[A,Vector[Edge[A]]] = Map.empty):

  def add(edge: Edge[A]): Graph[A] =
    Graph(neighbours.updated(edge.from, neighbours.getOrElse(edge.from, Vector.empty) :+ edge))

  def bidirectional(edge: Edge[A]): Graph[A] =
    add(edge).add(edge.inverse)

object Graph:

  def empty[A]: Graph[A] =
    Graph(Map.empty)

  def fromGrid[A](grid: Grid[A], node: A): Graph[Pos] =
    grid.elements.filter(_.element == node).foldLeft(Graph.empty): (g, f) =>
      f.pos.adjWithinGrid(grid).filter(grid.contains(_, node))
        .foldLeft(g): (g, p) =>
          g.add(Edge(f.pos, p, 1))


object Dijkstra:

  import scala.collection.mutable

  type Dist[A] = (A,Int)

  given sortByDistance[A]: Ordering[Dist[A]] =
    (d1, d2) => d1._2.compareTo(d2._2)

  def run[A](graph: Graph[A], from: A): Result[A] =
    val edgeTo: mutable.Map[A, Edge[A]] = mutable.Map.empty
    val distTo: mutable.Map[A, Int] = mutable.Map.from(graph.neighbours.map((node,_) => node -> Int.MaxValue))

    distTo += from -> 0
    val sourceEdge = from -> distTo(from)
    val queue = mutable.PriorityQueue[(A,Int)](sourceEdge)

    while (queue.nonEmpty)
      val (minDistNode, _) = queue.dequeue()
      val edges = graph.neighbours.getOrElse(minDistNode, Vector.empty)

      edges.foreach: edge =>
        if distTo(edge.to) > distTo(edge.from) + edge.distance then
          distTo.update(edge.to, distTo(edge.from) + edge.distance)
          edgeTo.update(edge.to, edge)
          if !queue.exists((node,_) => node == edge.to) then
            queue.enqueue((edge.to, distTo(edge.to)))

    Result(edgeTo.toMap, distTo.toMap)

  extension [A](path: Vector[Edge[A]])
    def toTrail: Vector[A] =
      if path.isEmpty then
        Vector.empty
      else
        path.foldLeft(Vector.empty[A])(_ :+ _.from) :+ path.last.to

case class Result[A](edgeTo: Map[A,Edge[A]], distancesTo: Map[A,Int]):

  def pathTo(node: A): Vector[Edge[A]] =
    @tailrec
    def go(edges: Vector[Edge[A]], node: A): Vector[Edge[A]] =
      edgeTo.get(node) match
        case Some(edge) => go(edge +: edges, edge.from)
        case None => edges

    if !hasEdge(node) then Vector.empty else go(Vector.empty, node)


  def hasEdge(node: A): Boolean =
    distancesTo.get(node).map(_ < Int.MaxValue).isDefined

  def distanceTo(node: A): Option[Int] =
    distancesTo.get(node).filter(_ < Int.MaxValue)
