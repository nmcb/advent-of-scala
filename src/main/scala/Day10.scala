import scala.annotation.tailrec
import scala.io.Source

object Day10 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  case class Circle(sparseHash: Vector[Int], current: Int, skipSize: Int):

    private inline def indexPlus(addition: Int) =
      (current + addition) % sparseHash.size

    infix def reverse(length: Int): Circle =
      val buffer = sparseHash.toBuffer
      for
        i <- 0 until length
      do
        val swap = indexPlus(i)
        val by   = indexPlus(length - i - 1)
        buffer(swap) = sparseHash(by)

      val nextSparseHash = buffer.toVector
      val nextCurrent    = indexPlus(length + skipSize)
      val nextSkipSize   = skipSize + 1
      Circle(nextSparseHash, nextCurrent, nextSkipSize)

    @tailrec
    final def calculate(runs: Int, lengths: Vector[Int]): Circle =
      if runs <= 0 then
        this
      else
        lengths.foldLeft(this)(_ reverse _).calculate(runs - 1, lengths)

    def productOfFirstTwo: Int =
      sparseHash.take(2).product

    def denseHash: Vector[Int] =
      sparseHash.grouped(16).map(_.reduce(_ ^ _)).toVector

    def hash: String =
      denseHash.map(_.toHexString).mkString("")

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
  val answer1: Int = Circle.init.calculate(1, lengths).productOfFirstTwo
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val input: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .map(_.toInt)
      .toVector ++ Vector(17, 31, 73, 47, 23)

  val start2: Long = System.currentTimeMillis
  val answer2: String = Circle.init.calculate(64, input).hash
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
