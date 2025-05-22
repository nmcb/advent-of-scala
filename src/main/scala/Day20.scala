import nmcb.*
import scala.io.*
import scala.annotation.*

import Option.*

object Day20 extends App:

  val day: String   = getClass.getName.filter(_.isDigit).mkString("")
  val input: String = Source.fromResource(s"input$day.txt").mkString.trim

  def cheats(path: Vector[Pos], timeframe: Long): Long =
    @tailrec
    def trace(trail: Vector[(Pos,Int)], cheated: Long): Long =
      if trail.nonEmpty then
        val (from,time) = trail.head
        val rest        = trail.tail
        val saved = rest.flatMap((to,left) => when(from.manhattan(to) <= timeframe)(left - time - from.manhattan(to)))
        trace(rest, cheated + saved.count(_ >= 100))
      else
        cheated

    trace(path.zipWithIndex, 0L)

  val path: Vector[Pos] = Grid.fromLines(input.linesIterator).extractPath('S', 'E', '.').shortest

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = cheats(path, 2)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = cheats(path, 20)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
