import scala.io.*

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val numbers: Seq[Int] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map:
          case s"$num" => num.toInt
        .toSeq

  lazy val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = numbers.min
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  lazy val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = numbers.max
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
