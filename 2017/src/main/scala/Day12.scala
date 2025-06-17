import scala.io.Source

object Day12 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Graph[N] = Map[N,Set[N]]

  val graph: Graph[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"$node <-> $children"
          => node.toInt -> children.trim.split(",").map(_.trim.toInt).toSet
      .toMap

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Dijkstra.reachable(0, graph).size
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  extension [N](graph: Graph[N]) def groups: Int =
    def loop(todo: Graph[N] = graph, result: Int = 0): Int =
      if todo.isEmpty then
        result
      else
        val node      = todo.keys.head
        val reachable = Dijkstra.reachable(node, todo)
        loop(todo.removedAll(reachable), result + 1)
    loop()

  val start2: Long = System.currentTimeMillis
  val answer2: Int = graph.groups
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

  object Dijkstra:
    import scala.collection.*
    def reachable[N](from: N, graph: Graph[N]): immutable.Set[N] =
      val found = mutable.Set.empty[N]
      val todo  = mutable.Queue.empty[N]
      todo.enqueue(from)
      while todo.nonEmpty do
        val node = todo.dequeue
        if !found.contains(node) then
          found += node
          graph(node).iterator.foreach(todo.enqueue)
      found.toSet