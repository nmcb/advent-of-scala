import scala.util.*
import scala.annotation.*
import scala.io.*

object SecurityExercise extends App:

  val routes: Vector[Route] =
    Source
      .fromResource(s"route-report.txt")
      .getLines
      .flatMap(Route.fromString)
      .toVector

  type Vertex = Set[String]

  case class Route(a: String, b: String):
    def vertices: Set[Vertex] = Set(Set(a), Set(b))

  object Route:
    def fromString(s: String): Set[Route] =
      s match
        case s"$a: $b" =>
          b.split(" ").map((b: String) => Route(a, b)).toSet

    extension (self: Vector[Route])
      def vertices: Set[Vertex] =
        self.flatMap(_.vertices).toSet

      def minCut(cardinality: Int)(using r: Random): (Set[Vertex], Vector[Route]) =
        @tailrec def loop(vertices: Set[Vertex], edges: Vector[Route], i: Int = 1): (Set[Vertex], Vector[Route]) =
          val (v: Set[Vertex], e: Vector[Route]) = Karger.karger(vertices, edges)
          if e.size == cardinality then (v, e) else loop(vertices, r.shuffle(edges), i + 1)
        loop(self.vertices, self)

  object Karger:
    @tailrec def karger(vertices: Set[Vertex], edges: Vector[Route]): (Set[Vertex], Vector[Route]) =
      if vertices.size <= 2 then (vertices, edges)
      else
        val Route(a: String, b: String) = edges.head
        val va: Vertex = vertices.find(_.contains(a)).get
        val vb: Vertex = vertices.find(_.contains(b)).get
        val vc: Vertex = va ++ vb

        val nextVertices: Set[Vertex] =
          vertices - va - vb + vc

        val nextRoutes: Vector[Route] =
          edges.filterNot:
            case Route(a, b) => vc.contains(a) && vc.contains(b)

        karger(nextVertices, nextRoutes)

  given Random = Random(2023_12_25)

  val (vertices: Set[Vertex], _) = routes.minCut(3)

  println(vertices.head.size)
  println(vertices.last.size)