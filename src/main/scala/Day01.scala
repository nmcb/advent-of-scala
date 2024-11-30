import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val values: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    666

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


