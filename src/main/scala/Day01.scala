import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val start1: Long =
    System.currentTimeMillis

  val values: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  def recover1(s: String): Int =
    val l = s.dropWhile(!_.isDigit).head.toString
    val r = s.reverse.dropWhile(!_.isDigit).head.toString
    (l + r).toInt

  val answer1: Int =
    values.map(recover1).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  def recover2(s: String): Int =

    val patterns: Map[String, String] =
      Map(
        "1" -> "1",
        "2" -> "2",
        "3" -> "3",
        "4" -> "4",
        "5" -> "5",
        "6" -> "6",
        "7" -> "7",
        "8" -> "8",
        "9" -> "9",
        "0" -> "0",
        "one" -> "1",
        "two" -> "2",
        "three" -> "3",
        "four" -> "4",
        "five" -> "5",
        "six" -> "6",
        "seven" -> "7",
        "eight" -> "8",
        "nine" -> "9",
        "zero" -> "0"
      )

    def left(todo: String): String =
      patterns.find((p,d) => todo.startsWith(p))
        .map((_,d) => d)
        .getOrElse(left(todo.tail))

    def right(todo: String): String =
      patterns
        .find((p,d) => todo.reverse.startsWith(p.reverse))
        .map((_,d) => d)
        .getOrElse(right(todo.init))

    (left(s) + right(s)).toInt

  val answer2: Int =
    values.map(recover2).sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
