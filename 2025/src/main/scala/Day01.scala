import scala.io.*

object Day01 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: Vector[String] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toVector
  
  def solve1(input: Vector[String]) =
    input
      .scanLeft(50): (last, rotation) =>
        rotation match
          case s"R$count" => last + count.toInt
          case s"L$count" => last - count.toInt
      .count(step => step % 100 == 0)

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = solve1(input)
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def solve2(input: Vector[String]) =
    input
      .flatMap:
          case s"R$count" => Vector.fill(count.toInt)(1)
          case s"L$count" => Vector.fill(count.toInt)(-1)
          case code       => sys.error(s"invalid code: $code")
      .scanLeft(50)(_ + _)
      .count(step => step % 100 == 0)

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = solve2(input)
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
