import scala.io._

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val start1: Long =
    System.currentTimeMillis

  val calories: Array[Array[Int]] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n").map(_.toInt))

  val answer1: Int =
    calories.map(_.sum).max

  println(s"Answer AOC 2022 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    calories.map(_.sum).sorted.takeRight(3).sum

  println(s"Answer AOC 2022 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
