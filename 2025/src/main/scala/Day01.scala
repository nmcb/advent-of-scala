import scala.io.*

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: Vector[String] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector

  extension (current: Int)
    infix def next(step: Char): Int =
      step match
        case 'L' if current == 0 => 99
        case 'L' => current - 1
        case 'R' if current == 99 => 0
        case 'R' => current + 1

  def solve1(input: Vector[String], start: Vector[Int] = Vector(50)) =
    input.foldLeft(start): (trace, rotation) =>
      rotation match
        case s"R$count" => trace :+ List.fill(count.toInt)('R').foldLeft(trace.last)(_ next _)
        case s"L$count" => trace :+ List.fill(count.toInt)('L').foldLeft(trace.last)(_ next _)

  lazy val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = solve1(input).count(_ == 0)
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(input: Vector[String], start: Vector[Int] = Vector(50)) =
    input
      .foldLeft(""): (result, rotation) =>
        rotation match
          case s"R$count" => result + ("R" * count.toInt)
          case s"L$count" => result + ("L" * count.toInt)
      .foldLeft(start): (trace, step) =>
        trace :+ trace.last.next(step)

  lazy val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = solve2(input).count(_ == 0)
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
