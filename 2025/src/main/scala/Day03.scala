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
      var result = ""
      var startIndex  = 0

      for
        remaining <- digits until 0 by -1
      do
        val endIndex = bank.length - remaining + 1

        var maxDigit = '0'
        var maxIndex = startIndex

        for
          index <- startIndex until endIndex
        do
          if bank(index) > maxDigit then
            maxDigit = bank(index)
            maxIndex = index

        result = result + maxDigit
        startIndex = maxIndex + 1

      result.toLong

  val start1: Long       = System.currentTimeMillis
  lazy val answer1: Long = input.map(_.maxJoltage(2)).sum
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long       = System.currentTimeMillis
  lazy val answer2: Long = input.map(_.maxJoltage(12)).sum
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
