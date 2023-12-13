import scala.annotation.tailrec
import scala.collection.*
import scala.io.*

object Day13 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Grid(image: Vector[Vector[Char]], defects: Int = 0):
    lazy val sizeX: Int = image.map(_.size).max
    lazy val sizeY: Int = image.size

    override def toString: String =
      image.map(_.mkString).mkString("\n")

    def mirrorLinesAt(x: Int)(line: Vector[Char]): (Vector[Char], Vector[Char]) =
      val (left, right) = line.splitAt(x)
      val length        = left.length min right.length
      val toTheLeft     = left.reverse.take(length)
      val toTheRight    = right.take(length)
      (toTheLeft, toTheRight)

    def hasMirrorLineAt(x: Int): Boolean =
      image.map(mirrorLinesAt(x)).count(_ != _) == defects

    lazy val xMirrorLines: Vector[Int] =
      (1 until sizeX).filter(hasMirrorLineAt).toVector

    lazy val yMirrorLines: Vector[Int] =
      copy(image = image.transpose).xMirrorLines

    lazy val summarize: Int =
      val col = xMirrorLines.headOption.getOrElse(0)
      val row = yMirrorLines.headOption.getOrElse(0)
      col + (100 * row)

  object Grid:
    def fromString(ss: Array[String]): Grid =
      Grid(ss.map(_.toVector).toVector, 0)

  lazy val grids: Vector[Grid] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n"))
      .map(Grid.fromString)
      .toVector

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = grids.map(_.summarize).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = grids.map(_.copy(defects = 1)).map(_.summarize).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
