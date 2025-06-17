import scala.io.Source

object Day05 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  def opposites(a: Char, b: Char): Boolean =
    a != b && a.toLower == b.toLower

  def react(poly: String): String =
    poly
      .foldLeft(List.empty[Char]):
        case (a :: it, b) if opposites(a, b) => it
        case (init, b)                       => b :: init
      .reverse
      .mkString("")

  def solve1(poly: String): Int =
    react(poly).length

  def solve2(poly: String): Int =
    val reacted = react(poly)
    val units   = reacted.toLowerCase.toSet
    units.map(unit => reacted.filterNot(_.toLower == unit)).map(solve1).min

  lazy val polymer: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve1(polymer)
  println(s"Answer AOC 2018 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve2(polymer)
  println(s"Answer AOC 2018 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
