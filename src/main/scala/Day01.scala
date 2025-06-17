import scala.io._

object Day01 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit)

  val depths: List[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toInt)
      .toList
  
  def solve(l: List[Int]): Int =
    l.sliding(2).count(l => l(1) > l(0))

  val start1 = System.currentTimeMillis
  val answer1: Int = solve(depths)
  println(s"Day$day answer part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  val answer2: Int = solve(depths.sliding(3).map(_.sum).toList)
  println(s"Day$day answer part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
