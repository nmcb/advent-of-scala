package aoc2022

import nmcb.*

object Day12 extends AoC:

  val puzzle: List[(Int,Int,Char)] =
    lines
      .zipWithIndex
      .foldLeft(List.empty[(Int,Int,Char)]) { case (a, (r, y)) =>
        r.zipWithIndex.foldLeft(a){ case (aa,(c,x)) =>
          aa :+ (x,y,c)
        }
      }

  lazy val (graph, from, to): (Graph, Vertex, Vertex) =

    val maxX: Int = puzzle.maxBy(_._1)._1
    val maxY: Int = puzzle.maxBy(_._2)._2

    def neighbours(x: Int, y: Int): Seq[(Int,Int)] =
      Seq((-1,0),(1,0),(0,-1),(0,1))
        .map((dx,dy) => (x + dx, y + dy))
        .filterNot((nx,ny) => nx < 0 || nx > maxX || ny < 0 || ny > maxY)

    def addOrInit(t: Vertex, f: Vertex, w: Int)(m: Map[Vertex,Seq[Edge]]): Map[Vertex,Seq[Edge]] =
      val e = Edge(t, f, w)
      m.updatedWith(t)(_.map(_ :+ e).orElse(Some(List(e))))

    def char(v: Vertex): Char =
      puzzle.find((sx, sy, _) => sx == v.x && sy == v.y).map((_, _, c) => c).getOrElse(sys.error("boom!"))

    def height(c: Char): Int =
      if      c == 'S' then 0
      else if c == 'E' then 25
      else c.toInt - 97

    def weight(f: Vertex, t: Vertex): Option[Int] =
      val fh = height(char(f))
      val th = height(char(t))
      th.compare(fh) match
        case  1 => // th > fh
          if th - fh == 1 then Some(2) else None
        case  0 => // th = fh
          Some(1)
        case -1 => // th < fh
          Some(th - fh + 2)

    val g: Graph =
      val adjacent: Map[Vertex,Seq[Edge]] =
        puzzle.foldLeft(Map.empty[Vertex,Seq[Edge]]):
          case (a,(x,y,_)) =>
            val f = Vertex(x, y)
            neighbours(x, y).foldLeft(a):
              case (aa, (nx, ny)) =>
                val t = Vertex(nx, ny)
                weight(f, t).map(w => addOrInit(f, t, w)(aa)).getOrElse(aa)
      Graph(adjacent)

    val f: Vertex =
      val (x, y, _) = puzzle.filter(_._3 == 'S').head
      Vertex(x, y)

    val t: Vertex =
      val (x, y, _) = puzzle.filter(_._3 == 'E').head
      Vertex(x, y)

    (g, f, t)

  case class Vertex(x: Int, y: Int)

  case class Edge(from: Vertex, to: Vertex, weight: Int)

  case class Calc(edgeTo: Map[Vertex,Edge], weightTo: Map[Vertex,Int]):

    def pathTo(to: Vertex): Seq[Edge] =
      @scala.annotation.tailrec
      def loop(v: Vertex, edges: Seq[Edge] = Seq.empty): Seq[Edge] =
        edgeTo.get(v) match
          case Some(e) => loop(e.from, e +: edges)
          case None => edges
      if !hasPath(to) then Seq.empty else loop(to)

    def hasPath(to: Vertex): Boolean =
      weightTo.contains(to)

    def weightToV(to: Vertex): Int =
      weightTo(to)

  case class Graph(adjacent: Map[Vertex,Seq[Edge]] = Map.empty):

    def add(e: Edge): Graph =
      Graph(adjacent.updatedWith(e.from)(_.map(_ :+ e).orElse(Some(List(e)))))

    def run(from: Vertex): Calc =
      import scala.collection.mutable
      import scala.util.Try

      val edgeTo = mutable.Map.empty[Vertex, Edge]
      val distTo = mutable.Map.from(adjacent.map((f,_) => f -> Int.MaxValue))

      distTo(from) = 0
      val sourceDist = (from, distTo(from))
      val sortByDist: Ordering[(Vertex, Int)] = (a, b) => a._2.compareTo(b._2)
      val queue = mutable.PriorityQueue[(Vertex, Int)](sourceDist)(using sortByDist.reverse)

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
            case None =>
              () // nop - can happen when the weight is 'doctored'
        }
      }
      Calc(edgeTo.toMap, distTo.toMap)


  lazy val answer1: Int = graph.run(from).pathTo(to).length
  lazy val answer2: Long =
    val as: List[Vertex] = puzzle.filter((_, _, c) => c == 'a' || c == 'S').map((x, y, _) => Vertex(x,y))
    as.map(f => graph.run(f).pathTo(to).length).filterNot(_ == 0).min
