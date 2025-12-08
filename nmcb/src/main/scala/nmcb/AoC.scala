package nmcb

import scala.io.Source
import scala.util.Try

abstract class AoC:

  val day: String = getClass.getName.replace('.', '/').init

  lazy val answer1: Any
  lazy val answer2: Any

  lazy val input: String = Try(Source.fromResource(s"$day.txt").mkString.trim).getOrElse("")
  lazy val lines: Vector[String] = input.linesIterator.toVector

  def main(args: Array[String]): Unit =
    val start1: Long = System.currentTimeMillis
    println(s"Answer $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

    val start2: Long = System.currentTimeMillis
    println(s"Answer $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

