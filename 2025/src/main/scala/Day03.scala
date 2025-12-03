import scala.io.*

object Day03 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Bank    = String
  val input: Vector[Bank] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector

  extension (bank: Bank)

    def maxJoltage(digits: Int): Long =
      @annotation.tailrec
      def loop(todo: String, remaining: Int, accumulator: Long): Long =
        if remaining == 0 then
          accumulator
        else
          val digit = todo.substring(0, todo.length - remaining + 1).max
          val index = todo.indexOf(digit)
          loop(todo.substring(index + 1), remaining - 1, accumulator * 10 + digit.asDigit)

      loop(bank, digits, 0)

  val start1 = System.currentTimeMillis
  val answer1 = input.map(_.maxJoltage(2)).sum
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  val answer2 = input.map(_.maxJoltage(12)).sum
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
