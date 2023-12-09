import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day10 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  lazy val input: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val start1: Long = System.currentTimeMillis
  val answer1: Int = ???
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = ???
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
