import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day09 extends App:

  val day: String = this.getClass.getName.drop(3).init

  lazy val input: Vector[Vector[Int]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.split(' ').map(_.toInt).toVector)
      .toVector

  def extrapolate(line: Vector[Int]): Int =
    val diffs = line.tail.zip(line).map(_-_)
    if diffs.forall(_ == 0L) then line.last else line.last + extrapolate(diffs)

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = input.map(extrapolate).sum
  println(s"Answer AOC 2023 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = input.map(_.reverse).map(extrapolate).sum
  println(s"Answer AOC 2023 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
