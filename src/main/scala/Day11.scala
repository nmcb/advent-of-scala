import scala.io.*

object Day11 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val grid: String =
    Source.fromResource(s"input$day.txt").mkString

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grid.solve1
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = grid.solve2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
