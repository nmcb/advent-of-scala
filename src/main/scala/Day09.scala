import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day09 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  lazy val input: List[List[Int]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.split(' ').map(_.toInt).toList)
      .toList

  type Triangle = List[List[Int]]

  def triangle(todo: List[Int], acc: List[List[Int]] = List.empty): Triangle =
    if todo.forall(_ == 0) then acc :+ todo else triangle(todo.tail.zip(todo).map(_-_), acc :+ todo)

  def solve(ds: List[List[Int]])(extrapolation: Triangle => Int): Int =
    ds.map(line => triangle(line)).map(extrapolation).sum

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve(input)(_.map(_.last).foldRight(0)(_+_))
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve(input)(_.map(_.head).foldRight(0)(_-_))
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
