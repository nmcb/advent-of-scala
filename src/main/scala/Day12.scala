import scala.io.Source
import scala.util.Try

object Day12 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val input: List[(Int,Int,Char)] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .zipWithIndex
      .foldLeft(List.empty[(Int,Int,Char)]) { case (a, (r, y)) =>
        r.zipWithIndex.foldLeft(a){ case (aa,(c,x)) =>
          aa :+ (x,y,c)
        }
      }

  lazy val (graph, from, to): (Graph, Vertex, Vertex) =

    def height(c: Char): Int =
      if      c == 'S' then 0
      else if c == 'E' then 25
      else c.toInt - 97

    val maxX: Int = input.maxBy(_._1)._1
    val maxY: Int = input.maxBy(_._2)._2

    def neighbours(x: Int, y: Int): Seq[(Int, Int)] =
      Seq((-1, 0),(1, 0), (0, -1),(0, 1))
        .map((dx, dy) => (x + dx, y + dy))
        .filterNot((nx, ny) => nx < 0 || nx > maxX || ny < 0 || ny > maxY)

    def addOrInit(t: Vertex, f: Vertex, w: Int)(m: Map[Vertex,Seq[Edge]]): Map[Vertex,Seq[Edge]] =
      val e = Edge(t, f, w)
      m.updatedWith(t)(_.map(_ :+ e).orElse(Some(List(e))))



    val g: Graph =
      Graph(
        input.foldLeft(Map.empty[Vertex,Seq[Edge]]){ case (a,(x,y,c)) => {
          val f = Vertex(x, y)
          val fh = height(c)
          neighbours(x, y).foldLeft(a) { case (aa, (nx, ny)) => {
            val t = Vertex(nx, ny)
            val th = height(input.find((sx,sy,_) => sx == nx && sy == ny).getOrElse(sys.error("boom!"))._3)
            th.compare(fh) match
              case  1 => // th > fh
                if th - fh == 1 then addOrInit(f, t, 2)(aa) else aa
              case  0 => // th = fh
                addOrInit(f, t, 1)(aa)
              case -1 => // th < fh
                addOrInit(f, t, th - fh + 2)(aa)
          }}
        }})

    val f: Vertex =
      val (x, y, _) = input.filter(_._3 == 'S').head
      Vertex(x, y)

    val t: Vertex =
      val (x, y, _) = input.filter(_._3 == 'E').head
      Vertex(x, y)

    (g, f, t)

  case class Vertex(x: Int, y: Int)

  case class Edge(from: Vertex, to: Vertex, weight: Int)

  case class Calc(edgeTo: Map[Vertex, Edge], distTo: Map[Vertex, Int]):
    def pathTo(to: Vertex): Seq[Edge] =
      @scala.annotation.tailrec
      def go(edges: Seq[Edge], cur: Vertex): Seq[Edge] =
        edgeTo.get(cur) match {
          case Some(e) => go(e +: edges, e.from)
          case None => edges
        }
      if (!hasPath(to)) Seq.empty else go(Seq.empty, to)

    def hasPath(to: Vertex): Boolean =
      distTo.contains(to)

    def distToV(Vertex: Vertex): Int =
      distTo(Vertex)

  case class Graph(adjacent: Map[Vertex, Seq[Edge]] = Map.empty):

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
      val queue = mutable.PriorityQueue[(Vertex, Int)](sourceDist)(sortByDist)

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
              () // nop
        }
      }
      Calc(edgeTo.toMap, distTo.toMap)


  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    graph.run(from).pathTo(to).length

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  assert(answer1 == 383)

  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    val as: List[Vertex] = input.filter((_,_,c) => c == 'a' || c == 'S').map((x,y,_) => Vertex(x, y))
    as.map(f => graph.run(f).pathTo(to).length).filterNot(_ == 0).min

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
