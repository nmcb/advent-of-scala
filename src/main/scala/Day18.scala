import nmcb.*

import scala.annotation.*
import scala.io.*

object Day18 extends App:

  import Pos.*
  import Dijkstra.*

  val day: String =
    getClass.getName.drop(3).init

  val bytes  = Source.fromResource(s"input$day.txt").getLines.map(_.split(',')).map(_.toPos).toVector
  val memory = Grid.fill(71, 71, '.')

  val start1: Long = System.currentTimeMillis
  val answer1: Int =
    val fallen = bytes.take(1024).foldLeft(memory)(_.updated(_, '#'))
    val graph  = Graph.fromGrid(fallen, '.')
    val result = Dijkstra.run(graph, fallen.minPos)
    result.distanceTo(fallen.maxPos).get

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  @tailrec
  def loop(todo: Vector[Pos], grid: Grid[Char]): Pos =
    val test     = grid.updated(todo.head, '.')
    val graph    = Graph.fromGrid(test, '.')
    val distance = Dijkstra.run(graph, test.minPos).distanceTo(test.maxPos)
    if distance.isEmpty then loop(todo.tail, test) else todo.head

  val start2: Long = System.currentTimeMillis
  val answer2: Pos = loop(bytes.reverse, bytes.foldLeft(memory)(_.updated(_, '#')))

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")


