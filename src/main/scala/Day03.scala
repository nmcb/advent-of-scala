import scala.io.*

object Day03 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  private val input: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString

  def solve1(s: String, result: Int): Int =
    s match
      case "" =>
        result
      case s"mul($l,$r)$rest" if l.toIntOption.nonEmpty && r.toIntOption.nonEmpty =>
        solve1(rest, result + l.toInt * r.toInt)
      case _ =>
        solve1(s.tail, result)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve1(input, 0)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(s: String, a: Int, enabled: Boolean): Int =
    s match
      case "" =>
        a
      case s"mul($l,$r)$rest" if l.toIntOption.nonEmpty && r.toIntOption.nonEmpty && enabled =>
        solve2(rest, a + l.toInt * r.toInt, enabled)
      case s"don't()$rest" =>
        solve2(rest, a, false)
      case s"do()$rest" =>
        solve2(rest, a, true)
      case _ =>
        solve2(s.tail, a, enabled)

  val start2: Long = System.currentTimeMillis
  val answer2: Int = solve2(input, 0, true)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
