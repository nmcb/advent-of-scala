import scala.io.*
import scala.util.*

object Day09 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: String =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim

  val marker = """\((\d+)x(\d+)\)""".r

  def solve1(str: String): Long =
    marker.findFirstMatchIn(str) match
      case Some(m) =>
        val length = m.group(1).toInt
        val times  = m.group(2).toInt
        m.start + times * length + solve1(m.after.toString.substring(length))
      case None =>
        str.length

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve1(input)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(str: String): Long =
    marker.findFirstMatchIn(str) match
      case Some(m) =>
        val length  = m.group(1).toInt
        val times   = m.group(2).toInt
        val after   = m.after.toString
        val process = after.substring(0, length)
        m.start + times * solve2(process) + solve2(after.substring(length))
      case None =>
        str.length

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve2(input)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")