import scala.annotation.tailrec
import scala.collection.*
import scala.io.*

object Day13 extends App:

  val day: String = this.getClass.getName.drop(3).init

  case class Grid(underlying: Vector[Vector[Char]], smudge: Option[(Int,Int)]):
    lazy val sizeX: Int = underlying.map(_.size).max
    lazy val sizeY: Int = underlying.size

    lazy val image: Vector[Vector[Char]] =
      (for {
        y <- 0 until sizeY
      } yield (for {
        x <- 0 until sizeX
      } yield
          smudge match
            case Some((sx, sy)) if sx == x && sy == y => if underlying(y)(x) == '.' then '#' else '.'
            case _                                    => underlying(y)(x))
        .toVector)
        .toVector

    override def toString: String =
      underlying.map(_.mkString).mkString("\n")

    def mirrorLinesAt(x: Int)(line: Vector[Char]): (Vector[Char], Vector[Char]) =
      val (left, right) = line.splitAt(x)
      val length = left.length min right.length
      val l = left.reverse.take(length)
      val r = right.take(length)
      (l, r)


    def mirrorLine(x: Int): Boolean =
      image.map(mirrorLinesAt(x)).forall(_ == _)

    lazy val mirrorsX: Vector[Int] =
      (1 until sizeX).filter(mirrorLine).toVector

    lazy val mirrorsY: Vector[Int] =
      Grid(underlying.transpose, smudge.map(_.swap)).mirrorsX

    lazy val summarize: Int =
      val col = mirrorsX.headOption.getOrElse(0)
      val row = mirrorsY.headOption.getOrElse(0)
      col + (100 * row)

    lazy val summarizeCorrected: Int =
      val corrected: Grid =
        (for {
          y <- 0 until sizeY
          x <- 0 until sizeX
        } yield Grid(underlying, Some((x, y))))
          .find: (option: Grid) =>
            val col = option.mirrorsX.diff(mirrorsX)
            val row = option.mirrorsY.diff(mirrorsY)
            col.nonEmpty || row.nonEmpty
          .getOrElse:
            sys.error("not found")

      val col = corrected.mirrorsX.diff(mirrorsX).headOption.getOrElse(0)
      val row = corrected.mirrorsY.diff(mirrorsY).headOption.getOrElse(0)

      col + (100 * row)

  object Grid:
    def fromString(ss: Array[String]): Grid =
      Grid(ss.map(_.toVector).toVector, None)

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
  val answer2: Long = grids.map(_.summarizeCorrected).sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
