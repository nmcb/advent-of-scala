import scala.annotation.tailrec
import scala.io.Source

object Day20 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val lines: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines()
      .toList

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = ???
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = ???
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")