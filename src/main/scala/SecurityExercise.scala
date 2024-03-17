import scala.util.*
import scala.annotation.*
import scala.io.*

/** @see https://en.wikipedia.org/wiki/Karger%27s_algorithm */
object SecurityExercise extends App:

  type Node    = String
  type Cluster = Set[Node]

  case class Route(a: Node, b: Node):
    def distinct: Set[Cluster] = Set(Set(a), Set(b))

  val routes: Vector[Route] =
    Source
      .fromResource(s"route-report.txt")
      .getLines
      .flatMap(Route.fromString)
      .toVector

  object Route:
    def fromString(s: String): Set[Route] =
      s match
        case s"$from: $tos" =>
          tos.split(" ").map((to: Node) => Route(from, to)).toSet

    extension (self: Vector[Route])
      def minCut(cardinality: Int)(using r: Random): (Set[Cluster], Vector[Route]) =
        @tailrec def loop(clusters: Set[Cluster], routes: Vector[Route]): (Set[Cluster], Vector[Route]) =
          val (cs: Set[Cluster], rs: Vector[Route]) = Karger.karger(clusters, routes)
          if rs.size == cardinality then (cs, rs) else loop(clusters, r.shuffle(routes))
        val distinct: Set[Cluster] = self.flatMap(_.distinct).toSet
        loop(distinct, self)

  object Karger:
    @tailrec def karger(clusters: Set[Cluster], routes: Vector[Route]): (Set[Cluster], Vector[Route]) =
      if clusters.size <= 2 then
        (clusters, routes)
      else
        val Route(a: Node, b: Node) = routes.head
        val va: Cluster = clusters.find(_.contains(a)).get
        val vb: Cluster = clusters.find(_.contains(b)).get
        val vc: Cluster = va ++ vb
        val nextClusters: Set[Cluster] = clusters - va - vb + vc
        val nextRoutes: Vector[Route]  = routes.filterNot(r => vc.contains(r.a) && vc.contains(r.b))
        karger(nextClusters, nextRoutes)

  given Random = Random(1)
  val (clusters: Set[Cluster], _) = routes.minCut(3)
  val cluster0 = clusters.head.size
  val cluster1 = clusters.last.size

  println(cluster0)
  println(cluster1)
