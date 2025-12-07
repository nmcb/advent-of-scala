package aoc2022

import nmcb.*

import scala.io.*

object Day04 extends AoC:

  val sections: List[(Range,Range)] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .map { case s"$l1-$r1,$l2-$r2" => (l1.toInt to r1.toInt, l2.toInt to r2.toInt) }
      .toList

  def contained1(r1: Range, r2: Range): Boolean =
    r1.forall(i => r2.contains(i)) | r2.forall(i => r1.contains(i))

  def contained2(r1: Range, r2: Range): Boolean =
    r1.exists(i => r2.contains(i))


  lazy val answer1: Int = sections.count(contained1)
  lazy val answer2: Int = sections.count(contained2)
