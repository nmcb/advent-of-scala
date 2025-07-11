import scala.annotation.*
import scala.io.*
import scala.util.*

/** @see https://en.wikipedia.org/wiki/Karger%27s_algorithm */
object SecurityExercise extends App:

  type Node    = String
  type Cluster = Set[Node]

  case class Route(a: Node, b: Node)

  object Route:
    def fromString(s: String): Set[Route] =
      s match
        case s"$from: $tos" =>
          tos.split(" ").map(to => Route(from, to)).toSet

  val routes: Vector[Route] =
    Source
      .fromResource(s"route-report.txt")
      .getLines
      .flatMap(Route.fromString)
      .toVector

  extension (routes: Vector[Route])

    @tailrec def karger(clusters: Set[Cluster]): (Set[Cluster], Vector[Route]) =
      if clusters.size <= 2 then
        (clusters, routes)
      else
        val Route(a: Node, b: Node) = routes.head
        val va: Cluster = clusters.find(_.contains(a)).get
        val vb: Cluster = clusters.find(_.contains(b)).get
        val vc: Cluster = va ++ vb
        val nextClusters: Set[Cluster] = clusters - va - vb + vc
        val nextRoutes: Vector[Route] = routes.filterNot(r => vc.contains(r.a) && vc.contains(r.b))
        nextRoutes.karger(nextClusters)

    def minCut(cardinality: Int)(using r: Random): (Set[Cluster], Vector[Route]) =
      @tailrec def loop(clusters: Set[Cluster], routes: Vector[Route]): (Set[Cluster], Vector[Route]) =
        val (cs: Set[Cluster], rs: Vector[Route]) = routes.karger(clusters)
        if rs.size == cardinality then (cs, rs) else loop(clusters, r.shuffle(routes))

      val clusters: Set[Cluster] = routes.flatMap(r => Set(Set(r.a), Set(r.b))).toSet
      loop(clusters, routes)

  given Random = Random(1)
  private val (clusters: Set[Cluster], cuts: Vector[Route]) = routes.minCut(3)
  val cluster0 = clusters.head.size
  val cluster1 = clusters.last.size

  println(cuts)
  println(cluster0)
  println(cluster1)
