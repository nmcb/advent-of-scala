import scala.io.Source

object Day18 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  def next(row: String): String =
    ("." + row + ".")
      .sliding(3)
      .map:
        case "^^." | ".^^" | "^.." | "..^" => '^'
        case _                             => '.'
      .mkString

  def safe(row: String): Int =
    row.count(_ == '.')

  val input   = Source.fromResource(s"input$day.txt").mkString.trim

  val start1  = System.currentTimeMillis
  val answer1 = Iterator.iterate(input)(next).map(safe).take(40).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = Iterator.iterate(input)(next).map(safe).take(400000).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

