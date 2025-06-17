import scala.io.*

object Day06 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type CharCount = (Char,Int)

  extension (cc: CharCount)
    def char: Char = cc._1
    def count: Int = cc._2

  case class Counter(counts: Map[Char,Int] = Map.empty):

    infix def add(char: Char): Counter =
      Counter(
        counts.updatedWith(char):
          case Some(count) => Some(count + 1)
          case None        => Some(1)
      )

    def max: Char =
      counts.toVector.maxBy(_.count).char

    def min: Char =
      counts.toList.minBy(_.count).char

  object Counter:

    val empty: Counter =
      Counter()

  val input: Vector[String] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .toVector

  def counted(input: Vector[String]): Vector[Counter] =
    input
      .map(_.toVector)
      .transpose
      .map(_.foldLeft(Counter.empty)(_ add _))

  val start1  = System.currentTimeMillis
  val answer1 = counted(input).map(_.max).mkString
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = counted(input).map(_.min).mkString
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")