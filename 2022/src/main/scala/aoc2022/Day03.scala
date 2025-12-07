package aoc2022

import nmcb.*

import scala.io.*

object Day03 extends AoC:

  val items: List[String] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .toList

  def priority(c: Char): Int =
    if c.isLower then c.toInt - 96 else c.toInt - 38

  def priority(line: String): Int =
    assert(line.length % 2 == 0)
    val left  = line.take(line.length / 2)
    val right = line.takeRight(line.length / 2)
    val List(c) = left.toSet.intersect(right.toSet).toList
    priority(c)

  def priority(lines: List[String]): Int =
    assert(lines.length == 3)
    assert(lines.forall(_.length % 2 == 0))
    val List(r1, r2, r3) = lines
    val List(c) = r1.toSet.intersect(r2.toSet).intersect(r3.toSet).toList
    priority(c)


  lazy val answer1: Int = items.map(priority).sum
  lazy val answer2: Int = items.grouped(3).map(priority).sum
