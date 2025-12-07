import scala.annotation.*
import scala.io.*

object Day03 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit).mkString("")

  val Zero = '0'
  val One  = '1'

  type Bits = List[Char]

  val diagnostics: List[Bits] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toList)
      .toList

  extension (bits: Bits)

    def mostCommon: Char =
      if bits.count(_ == Zero) > bits.count(_ == One) then Zero else One

    def leastCommon: Char =
      if bits.count(_ == Zero) > bits.count(_ == One) then One else Zero

    def toInt: Int =
      Integer.parseInt(bits.mkString, 2)


  val start1  = System.currentTimeMillis

  val gamma   = diagnostics.transpose.map(_.mostCommon).toInt
  val epsilon = diagnostics.transpose.map(_.leastCommon).toInt
  lazy val answer1 = gamma * epsilon
  
  println(s"Answer AOC 2020 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def rating(bits: List[Bits], commonOf: Bits => Char): Int =
    @tailrec
    def filter(todo: List[Bits], idx: Int = 0): Bits =
      if todo.size == 1 then
        todo.head
      else
        val common = commonOf(todo.transpose.apply(idx))
        val next   = todo.filter(bit => bit(idx) == common)
        filter(next, idx + 1)
    filter(bits).toInt

  val start2 = System.currentTimeMillis

  val oxygenRating       = rating(diagnostics, _.mostCommon)
  val co2SchrubberRating = rating(diagnostics, _.leastCommon)
  lazy val answer2            = oxygenRating * co2SchrubberRating

  println(s"Answer AOC 2020 day $day part 1: $answer2 [${System.currentTimeMillis - start2}ms]")
