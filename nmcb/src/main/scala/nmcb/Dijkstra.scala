package nmcb

import pos.*
import predef.*

import scala.annotation.*

object Dijkstra:

  final case class Edge[A](from: A, to: A, weight: Int):
    def inverse: Edge[A] = Edge(to, from, weight)
    
  object Edge:
    def unit[A](from: A, to: A): Edge[A] =
      Edge(from, to, 1)

  final case class Graph[A](neighbours: Map[A,Vector[Edge[A]]] = Map.empty):

    def add(edge: Edge[A]): Graph[A] =
      Graph(neighbours.updated(edge.from, neighbours.getOrElse(edge.from, Vector.empty) :+ edge))

    def bidirectional(edge: Edge[A]): Graph[A] =
      add(edge).add(edge.inverse)

  object Graph:

    def empty[A]: Graph[A] =
      Graph(Map.empty)

    def fromEdges[A](edges: Vector[Edge[A]]): Graph[A] =
      Graph(edges.groupMap(_.from)(identity))

    def fromGrid[A](grid: Grid[A], node: A, dist: (Pos,Pos) => Int = (_,_) => 1): Graph[Pos] =
      grid.elements
        .filter(_.element == node)
        .foldLeft(Graph.empty): (graph,from) =>
          from.pos
            .adjWithinGrid[A](grid, _.element == node)
            .filter(p => grid.contains(p, node))
            .foldLeft(graph): (graph, to) =>
              graph.add(Edge(from.pos, to, dist(from.pos, to)))


  case class Result[A](edgeTo: Map[A,Edge[A]], distancesTo: Map[A,Int]):

    def pathTo(node: A): Vector[Edge[A]] =
      @tailrec
      def build(node: A, edges: Vector[Edge[A]] = Vector.empty): Vector[Edge[A]] =
        edgeTo.get(node) match
          case Some(edge) => build(edge.from, edge +: edges)
          case None       => edges
      if hasEdge(node) then build(node) else Vector.empty

    def hasEdge(node: A): Boolean =
      distancesTo.get(node).map(_ < Int.MaxValue).isDefined

    def distanceTo(node: A): Option[Int] =
      distancesTo.get(node).filter(_ < Int.MaxValue)


  import scala.collection.mutable

  /** classic dijkstra traverses the shortest found paths first */
  def run[A](graph: Graph[A], from: A): Result[A] =
    val edgeTo: mutable.Map[A,Edge[A]] = mutable.Map.empty
    val distTo: mutable.Map[A,Int]     = mutable.Map.from(graph.neighbours.map((node,_) => node -> Int.MaxValue))

    distTo += from -> 0
    val sourceEdge = from -> distTo(from)
    val queue = mutable.PriorityQueue[(A,Int)](sourceEdge)(using Ordering.by[(A,Int),Int](_._2).reverse)

    while (queue.nonEmpty)
      val (minDistNode, _) = queue.dequeue()
      val edges = graph.neighbours.getOrElse(minDistNode, Vector.empty)
      edges.foreach: edge =>
        if distTo(edge.to) > distTo(edge.from) + edge.weight then
          distTo.update(edge.to, distTo(edge.from) + edge.weight)
          edgeTo.update(edge.to, edge)
          if !queue.exists((node,_) => node == edge.to) then
            queue.enqueue((edge.to, distTo(edge.to)))

    Result(edgeTo.toMap, distTo.toMap)

  /** breadth first flooding */
  def reachable[N](start: N, edgesFrom: N => Set[N]): Set[N] =
    val found = mutable.Set.empty[N]
    val todo  = mutable.Queue.empty[N]
    todo.enqueue(start)
    while todo.nonEmpty do
      val from = todo.dequeue
      if !found.contains(from) then
        found += from
        edgesFrom(from).iterator.foreach: to =>
          if !found.contains(to) then
            todo.enqueue(to)
    found.toSet


  def breadthFirstSearch[A,B](a: A)(f: A => Either[Set[A],B]): Vector[B] =
    val results = mutable.ArrayBuffer.empty[B]
    val queue   = mutable.Queue(a)
    while queue.nonEmpty do
      f(queue.dequeue) match
        case Right(result)    => results += result
        case Left(neighbours) => queue.enqueueAll(neighbours)
    results.toVector


  extension [A](path: Vector[Edge[A]])
    def toTrail: Vector[A] =
      if path.isEmpty then
        Vector.empty
      else
        path.foldLeft(Vector.empty[A])(_ :+ _.from) :+ path.last.to
