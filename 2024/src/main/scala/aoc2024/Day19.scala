package aoc2024

import nmcb.*
import nmcb.predef.*

object Day19 extends AoC:

  case class Design(stripes: String)

  val (towels: Vector[String], designs: Vector[String]) =
    val Array(ts, ds) = input.split("\n\n").map(_.trim)
    (ts.split(',').map(_.trim).toVector, ds.linesIterator.toVector)

  def count(towels: Vector[String], target: String): Long =
    
    val cache = memo("" -> 1L)
    def loop(remaining: String): Long = cache.memoize(remaining):
        towels
          .filter(remaining.startsWith)
          .map(t => loop(remaining.drop(t.length)))
          .sum
    loop(target)

  lazy val answer1: Long = designs.map(d => count(towels, d)).count(_ > 0)
  lazy val answer2: Long = designs.map(d => count(towels, d)).sum
