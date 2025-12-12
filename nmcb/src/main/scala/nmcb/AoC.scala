package nmcb

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

abstract class AoC:

  val day: String = getClass.getName.replace('.', '/').init

  lazy val answer1: Any
  lazy val answer2: Any

  lazy val input: String = Try(Source.fromResource(s"$day.txt").mkString.trim).getOrElse("")
  lazy val lines: Vector[String] = input.linesIterator.toVector
  lazy val chunks: Vector[Vector[String]] =
    @tailrec
    def recurse(xs: Vector[String], acc: Vector[Vector[String]] = Vector.empty): Vector[Vector[String]] =
      val (chunk, rest) = xs.span(_ != "")
      if chunk.isEmpty then acc.reverse
      else recurse(rest.drop(1), chunk +: acc)

    recurse(lines)


  def main(args: Array[String]): Unit =
    val start1: Long = System.currentTimeMillis
    println(s"Answer $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

    val start2: Long = System.currentTimeMillis
    println(s"Answer $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

