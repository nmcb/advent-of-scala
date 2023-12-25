import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object Day25 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Component = String
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

    def disconnect(c: Connection): Set[Connection] =
      cs.filterNot(_ == c).filterNot(_ == c.swap)

    def components: Set[Component] =
      cs.flatMap((a,b) => Set(a, b))


  // Human After All
  Dot.showDOT(connections)

  val ignoreTest = Vector("hfx" -> "pzl", "bvb" -> "cmg", "nvd" -> "jqt")
  val ignoreProd = Vector("xkz" -> "mvv", "gbc" -> "hxr", "tmt" -> "pnz")

  val remove = ignoreProd
  val purged = remove.foldLeft(connections)(_ disconnect _)
  val group0 = Dijkstra.reachable(remove(0)._1, purged.connectedTo)
  val group1 = purged.components -- group0

  val start1: Long = System.currentTimeMillis
  val answer1: Int = group0.size * group1.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = 50
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

  object Dijkstra:
    def reachable[A](start: A, neighbours: A => Set[A]): Set[A] =
      val found = mutable.Map.empty[A, Int]
      val todo = mutable.Queue.empty[(Int, A)]
      todo.enqueue((0, start))
      while todo.nonEmpty do
        val (dist, node) = todo.dequeue()
        if !found.contains(node) then
          found(node) = dist
          def process(newNode: A): Unit =
            if !found.contains(newNode) then
              val newDist = dist + 1
              todo.enqueue((newDist, newNode))
          neighbours(node).iterator.foreach(process)
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

      def edgeTransformer(innerEdge: Graph[String, WUnDiEdge[String]]#GraphInnerEdge): Option[(DotGraph, DotEdgeStmt)] =
        val edge = innerEdge.outer
        val label = edge.weight.toInt
        Some(root, DotEdgeStmt(
          NodeId(edge.source),
          NodeId(edge.target),
          List(DotAttr(Id("label"), Id(s"${edge.source}/${edge.target}")))
        ))

      val graph =
        Graph.from[Component, WUnDiEdge[Component]](
          connections.components,
          connections.map((a, b) => WUnDiEdge(a, b, 1))
        )

      val dot = graph.toDot(root, edgeTransformer)
      val fileWriter = new FileWriter(new File(s"/tmp/$prefix.dot"))
      fileWriter.write(dot)
      fileWriter.close()

    def showDOT(connections: Set[Connection]): Unit =
      writeDOT(connections)
      Runtime.getRuntime.exec(Array("neato", "-Tpdf", s"/tmp/$prefix.dot", "-o", s"/tmp/$prefix.pdf")).waitFor()
      Runtime.getRuntime.exec(Array("open", s"/tmp/$prefix.pdf")).waitFor()
