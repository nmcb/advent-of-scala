import scala.io._

object Day01 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  val values: List[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toList

  val digits: Map[String, Char] =
    Map(
      "1" -> '1',
      "2" -> '2',
      "3" -> '3',
      "4" -> '4',
      "5" -> '5',
      "6" -> '6',
      "7" -> '7',
      "8" -> '8',
      "9" -> '9',
      "0" -> '0'
    )

  def recover(replacements: Map[String, Char])(s: String): Int =

    def left(todo: String): Char =
      replacements.find((p, d) => todo.startsWith(p)).map(_._2).getOrElse(left(todo.tail))

    def right(todo: String): Char =
      replacements.find((p, d) => todo.endsWith(p)).map(_._2).getOrElse(right(todo.init))

    s"${left(s)}${right(s)}".toInt

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    values.map(recover(digits)).sum


  val start2: Long =
    System.currentTimeMillis

  val names: Map[String, Char] =
    Map(
      "one"   -> '1',
      "two"   -> '2',
      "three" -> '3',
      "four"  -> '4',
      "five"  -> '5',
      "six"   -> '6',
      "seven" -> '7',
      "eight" -> '8',
      "nine"  -> '9',
      "zero"  -> '0'
    )

  val answer2: Int =
    values.map(recover(digits ++ names)).sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
