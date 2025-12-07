package aoc2022

import nmcb.*
import scala.io.*
import scala.math.*

object Day16 extends AoC:

  case class Valve(name: Vertex, rate: Int, tunnels: List[Vertex], closed: Boolean = true)

  val valves: List[Valve] =
    Source
      .fromResource(s"$day.txt")
      .getLines()
      .toList
      .map {
        case s"Valve $name has flow rate=$rate; tunnels lead to valves $tunnels" =>
          Valve(name, rate.toInt, tunnels.trim.split(',').map(_.trim).toList)
        case s"Valve $name has flow rate=$rate; tunnel leads to valve $tunnel" =>
          Valve(name, rate.toInt, List(tunnel))
      }

  val reachability: Map[Vertex,List[Vertex]] =
    valves.map(v => v.name -> v.tunnels).toMap

  val pressures: Map[Vertex,Int] =
    valves.map(v => v.name -> v.rate).toMap

  val nonzero: List[Vertex] =
    valves.filter(_.rate != 0).map(_.name)

  val distances: Map[(Vertex,Vertex),Int] =
    val graph: Graph =
      reachability.foldLeft(Graph()) { case (g, (k, v)) =>
        v.foldLeft(g)((g, l) =>
          g.add(Edge(k, l, 1))
        )
      }
    val m = for {
      k1 <- valves.map(_.name)
      k2 <- valves.map(_.name)
      best = graph.run(k1).weightToV(k2)
    } yield (k1,k2) -> best
    m.toMap

  def solve1(time: Int, pressure: Int, flow: Int, current: Vertex, remaining: List[Vertex], limit: Int): Int =
    remaining.foldLeft(pressure + (limit - time) * flow): (max, v) =>
      val steps = distances((current, v)) + 1
      if (time + steps) < limit then
        val t = time + steps
        val p = pressure + (steps * flow)
        val f = flow + pressures(v)
        val score = solve1(t, p, f, v, remaining.filter(_ != v), limit)
        if score > max then
          score
        else
          max
      else
        max
  
  def solve2(): Int =
    (1 to nonzero.length / 2).foldLeft(0): (max,i) =>
      nonzero.combinations(i).foldLeft(max): (max,left) =>
        val max2a = solve1(0, 0, 0, "AA", left, 26)
        val right = nonzero.filterNot(v => left.contains(v))
        val max2b = solve1(0, 0, 0, "AA", right, 26)
        if max2a + max2b > max then max2a + max2b else max

  lazy val answer1: Int = solve1(0, 0, 0, "AA", nonzero, 30)
  lazy val answer2: Int = solve2()

  
  /** Utilities */

  type Vertex = String

  case class Edge(from: Vertex, to: Vertex, weight: Int)

  case class Calc(edgeTo: Map[Vertex, Edge], weightTo: Map[Vertex, Int]):
    def pathTo(to: Vertex): Seq[Edge] =
      @scala.annotation.tailrec
      def loop(v: Vertex, edges: Seq[Edge] = Seq.empty): Seq[Edge] =
        edgeTo.get(v) match {
          case Some(e) => loop(e.from, e +: edges)
          case None => edges
        }
      if (!hasPath(to)) Seq.empty else loop(to)

    def hasPath(to: Vertex): Boolean =
      weightTo.contains(to)

    def weightToV(to: Vertex): Int =
      weightTo(to)

  case class Graph(adjacent: Map[Vertex, Seq[Edge]] = Map.empty):

    def add(e: Edge): Graph =
      Graph(adjacent.updatedWith(e.from)(_.map(_ :+ e).orElse(Some(List(e)))))

    def run(from: Vertex): Calc =

      import scala.collection.mutable

      val edgeTo = mutable.Map.empty[Vertex, Edge]
      val distTo = mutable.Map.from(adjacent.map((f,_) => f -> Int.MaxValue))

      distTo(from) = 0
      val sourceDist = (from, distTo(from))
      val sortByDist: Ordering[(Vertex, Int)] = (a, b) => a._2.compareTo(b._2)
      val queue = mutable.PriorityQueue[(Vertex, Int)](sourceDist)(using sortByDist)

      while (queue.nonEmpty) {
        val (minDestV, _) = queue.dequeue()
        val edges = adjacent.getOrElse(minDestV, List.empty)

        edges.foreach { e =>
          distTo.get(e.to) match
            case Some(distToTo) =>
              val distToFrom = distTo(e.from)
              if distToTo > distToFrom + e.weight then
                distTo(e.to) = distToFrom + e.weight
                edgeTo(e.to) = e
                if !queue.exists(_._1 == e.to) then
                  queue.enqueue((e.to, distToTo))
            case None => ()
        }
      }
      Calc(edgeTo.toMap, distTo.toMap)
