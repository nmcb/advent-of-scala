import scala.annotation.tailrec
import scala.collection.*
import scala.io.*

object Day13 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Image(image: Vector[Vector[Char]], defects: Int = 0):

    def smudgeCorrection(on: Boolean): Image =
      copy(defects = if on then 1 else 0)

    private lazy val column: Int =
      (1 until image.last.size).find: n =>
        image.count: line =>
          val (l, r) = line splitAt n
          val length = l.length min r.length
          l.reverse.take(length) != r.take(length)
        == defects
      .getOrElse(0)

    private lazy val row: Int =
      copy(image.transpose).column

    lazy val summarize: Int =
      column + (100 * row)

  object Image:
    def fromString(ss: Array[String]): Image =
      Image(ss.map(_.toVector).toVector)

  lazy val images: Vector[Image] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n"))
      .map(Image.fromString)
      .toVector

  val start1: Long  = System.currentTimeMillis
  lazy val answer1: Long = images.map(_.summarize).sum
  println(s"Answer AOC 2023 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  lazy val answer2: Long = images.map(_.smudgeCorrection(true).summarize).sum
  println(s"Answer AOC 2023 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
