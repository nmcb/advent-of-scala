package aoc2024

import nmcb.*

object Day01 extends AoC:

  val (left, right): (Seq[Int], Seq[Int]) =
    val values = lines.map:
      case s"$l   $r" => (l.toInt, r.toInt)

    (values.map(_._1).sorted, values.map(_._2).sorted)

  lazy val answer1: Int = left.zip(right).map(_ - _).map(math.abs).sum
  lazy val answer2: Int = left.foldLeft(0)((a,l) => a + l * right.count(_ == l))
