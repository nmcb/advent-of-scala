import scala.io.Source

object Day12 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Graph = Map[Int,Set[Int]]

  val graph: Graph =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$node <-> $children"
          => node.toInt -> children.trim.split(",").map(_.trim.toInt).toSet
      .toMap

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Dijkstra.reachable(0, graph.apply).size
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def groups(graph: Graph, result: Int = 0): Int =
    if graph.keys.isEmpty then
      result
    else
      val node      = graph.keys.head
      val reachable = Dijkstra.reachable(node, graph.apply)
      groups(graph.removedAll(reachable), result + 1)

  val start2: Long = System.currentTimeMillis
  val answer2: Int = groups(graph)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

  object Dijkstra:
    import scala.collection.*
    def reachable[N](from: N, edges: N => Set[N]): Set[N] =
      val found = mutable.Map.empty[N,Int]
      val todo  = mutable.Queue.empty[(Int,N)]
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