import scala.io.*

object Day01 extends App:

  private val day: String =
    this.getClass.getName.drop(3).init

  private val input: Seq[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .map(_.toString.toInt)

  private def equal(d0: Int, d1: Int): Int =
    if d0 == d1 then d0 else 0

  private val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    input
      .zip(input.tail :+ input.head)
      .map(equal)
      .sum

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  private val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    assert(input.length % 2 == 0)
    val (msb, lsb) = input.splitAt(input.length / 2)
    input
      .zip(lsb ++ msb)
      .map(equal)
      .sum

  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
