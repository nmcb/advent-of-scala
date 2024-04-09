import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable
import scala.util.*

object Day25 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Component = String

  // todo experiment with `##` and `equals` to allow for unordered pairs
  type Connection = (Component, Component)

  val connections: Set[Connection] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .flatMap:
        case s"$name1: $connections" =>
          connections.split(" ").map(name2 => name1 -> name2)
      .toSet

  extension (cs: Set[Connection])

    def connectedTo(c: Component): Set[Component] =
      cs.filter(_._1 == c).map(_._2) ++ cs.filter(_._2 == c).map(_._1)

    infix def disconnect(c: Connection): Set[Connection] =
      cs.filterNot(_ == c)

    def components: Set[Component] =
      cs.flatMap((a,b) => Set(a, b))


  // Human After All
  Dot.showDOT(connections)

  val ignoreTest = Vector("pzl" -> "hfx", "cmg" -> "bvb", "jqt" -> "nvd") // hardcoded :)
  val ignoreProd = Vector("xkz" -> "mvv", "gbc" -> "hxr", "tmt" -> "pnz") // also hardcoded ;)

  val remove = ignoreProd
  val purged = remove.foldLeft(connections)(_ disconnect _)
  val group0 = Dijkstra.reachable(remove(0)._1, purged.connectedTo)
  val group1 = purged.components -- group0


  val start1: Long = System.currentTimeMillis
  val answer1: Int = group0.size * group1.size
  println(s"Answer day $day part 1: ${group0.size} * ${group1.size} = ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val totalStars2023: Int = 50
  println(s"Answer day $day part 2: ${totalStars2023} [${System.currentTimeMillis - start2}ms]")

  object Dijkstra:
    def reachable[N](from: N, edges: N => Set[N]): Set[N] =
      val found = mutable.Map.empty[N, Int]
      val todo  = mutable.Queue.empty[(Int, N)]
      todo.enqueue((0, from))
      while todo.nonEmpty do
        val (dist, node) = todo.dequeue()
        if !found.contains(node) then
          found(node) = dist
          def process(newNode: N): Unit =
            if !found.contains(newNode) then
              val newDist = dist + 1
              todo.enqueue((newDist, newNode))
          edges(node).iterator.foreach(process)
      found.keys.toSet


  /**
   * Thanks JP <3<3<3
   *
   * Usage: neato -Tpdf graph.dot -o graph.pdf
   */
  object Dot:

    import java.io.{File, FileWriter}
    import scalax.collection.edges.labeled.WUnDiEdge
    import scalax.collection.immutable.Graph
    import scalax.collection.io.dot.*
    import implicits.*

    val prefix = "graph"

    def writeDOT(connections: Set[Connection]): Unit =
      val root = DotRootGraph(directed = false, id = Some(Id("Day25")))

      def edgeTransformer(index: Map[Component,Int])(innerEdge: Graph[Int, WUnDiEdge[Int]]#GraphInnerEdge): Option[(DotGraph, DotEdgeStmt)] =
        val edge = innerEdge.outer
        val label = edge.weight.toInt
        Some(root, DotEdgeStmt(
          NodeId(edge.source),
          NodeId(edge.target),
          List() // (DotAttr(Id("label"), Id(s"${edge.source}/${edge.target}")))
        ))

      val index: Map[Component,Int] =
        connections.components.zipWithIndex.toMap

      val graph =
        Graph.from[Int, WUnDiEdge[Int]](
          index.values,
          connections.map((a, b) => WUnDiEdge(index(a) , index(b), 1))
        )

      val dot = graph.toDot(root, edgeTransformer(index))

      try
        val fileWriter = new FileWriter(new File(s"/tmp/$prefix.dot"))
        fileWriter.write(dot)
        fileWriter.close()
      catch
        case _ => println("unable to write dot file [ignoring]")


    def showDOT(connections: Set[Connection]): Unit =
      writeDOT(connections)
      try
        Runtime.getRuntime.exec(Array("neato", "-Tpdf", s"/tmp/$prefix.dot", "-o", s"/tmp/$prefix.pdf")).waitFor()
        Runtime.getRuntime.exec(Array("open", s"/tmp/$prefix.pdf")).waitFor()
      catch
        case _ => println(s"unable to show visualisation [ignoring]")
