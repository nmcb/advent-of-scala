import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day08 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val input: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  lazy val answer1: Int =
    ???

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  lazy val answer2: Int =
    ???

  println(s"Answer day $day part 1: ${answer2} [${System.currentTimeMillis - start2}ms]")
