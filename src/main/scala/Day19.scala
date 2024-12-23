import scala.collection.*
import scala.io.*

import nmcb.predef.*

object Day19 extends App:

  val day: String =
    getClass.getName.drop(3).init

  case class Design(stripes: String)

  val (towels: Vector[String], designs: Vector[String]) =
    val Array(ts, ds) = Source.fromResource(s"input$day.txt").mkString.split("\n\n").map(_.trim)
    (ts.split(',').map(_.trim).toVector, ds.linesIterator.toVector)

  def count(towels: Vector[String], target: String): Long =
    val cache = memo("" -> 1L)
    def loop(remaining: String): Long =
      cache.memoize(remaining):
        towels
          .filter(remaining.startsWith)
          .map(t => loop(remaining.drop(t.length)))
          .sum
    loop(target)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = designs.map(d => count(towels, d)).count(_ > 0)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = designs.map(d => count(towels, d)).sum
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
