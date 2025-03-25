import scala.io.Source

object Day10 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  case class Circle(underlying: Vector[Int], index: Int, skipSize: Int):

    extension (i: Int) private inline def toIndex =
      i % underlying.size

    infix def reverse(length: Int): Circle =
      val buffer = underlying.toBuffer
      for
        i <- 0 until length
      do
        val swap     = (index + i).toIndex
        val pointer  = (index + length - i - 1).toIndex
        buffer(swap) = underlying(pointer)

      val nextIndex    = (index + length + skipSize).toIndex
      val nextSkipSize = skipSize + 1
      Circle(buffer.toVector, nextIndex, nextSkipSize)

    def productOfFirstTwo: Int =
      underlying.take(2).product

  object Circle:
    def init: Circle =
      Circle(Vector.range(0, 256), 0, 0)

  val lengths: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .map(_.toInt)
      .toVector

  val start1: Long = System.currentTimeMillis
  val answer1: Int = lengths.foldLeft(Circle.init)(_ reverse _).productOfFirstTwo
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
