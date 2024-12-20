import nmcb.*
import scala.io.*
import scala.annotation.*

import Dijkstra.*
import Option.*

object Day20 extends App:

  val day: String =
    getClass.getName.drop(3).init

  case class Design(stripes: String)

  val racetrack = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)
  val start     = racetrack.findOne('S')
  val end       = racetrack.findOne('E')
  val updated   = racetrack.updated(start, '.').updated(end, '.')
  val graph     = Graph.fromGrid(updated, '.')
  val result    = Dijkstra.run(graph, start)
  val path      = result.pathTo(end).toTrail

  def cheats(path: Vector[Pos], timeframe: Long): Long =
    @tailrec
    def loop(trail: Vector[(Pos,Int)], cheated: Long): Long =
      if trail.nonEmpty then
        val (from,time) = trail.head
        val rest        = trail.tail
        val saved = rest.flatMap((to,left) => when(from.manhattan(to) <= timeframe)(left - time - from.manhattan(to)))
        loop(rest, cheated + saved.count(_ >= 100))
      else
        cheated

    loop(path.zipWithIndex, 0L)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = cheats(path, 2)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = cheats(path, 20)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
