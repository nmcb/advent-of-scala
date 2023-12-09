import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day09 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Input = Vector[Vector[Int]]

  lazy val input: Input =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.split(' ').map(_.toInt).toVector)
      .toVector

  type Triangle = Vector[Vector[Int]]

  def triangle(numbers: Vector[Int], acc: Triangle = Vector.empty): Triangle =
    if numbers.forall(_ == 0) then acc :+ numbers else triangle(numbers.tail.zip(numbers).map(_-_), acc :+ numbers)

  def solve(input: Input)(extrapolation: Triangle => Int): Int =
    input.map(numbers => triangle(numbers)).map(extrapolation).sum

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve(input)(_.map(_.last).foldRight(0)(_+_))
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(input)(_.map(_.head).foldRight(0)(_-_))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
