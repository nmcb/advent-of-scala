import scala.io.*

object Day07 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val positions: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(",")
      .map(_.toInt)
      .toVector

  extension (positions: Vector[Int])

    def fuelConsumptionTo1(target: Int): Int =
      positions.map(pos => (target - pos).abs).sum

    def fuelConsumptionTo2(target: Int): Int =
      positions.map(pos => (1 to (target - pos).abs).sum).sum

    def solve(fuelTo: Vector[Int] => Int => Int): Int =
      (positions.min to positions.max).map(fuelTo(positions)).min


  val start1 = System.currentTimeMillis
  lazy val answer1 = positions.solve(_.fuelConsumptionTo1)
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  lazy val answer2 = positions.solve(_.fuelConsumptionTo2)
  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
