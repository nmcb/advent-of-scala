package aoc2023

import nmcb.*

object Day09 extends AoC:

  lazy val matrix: Vector[Vector[Int]] = lines.map(_.split(' ').map(_.toInt).toVector)

  def extrapolate(line: Vector[Int]): Int =
    val diffs = line.tail.zip(line).map(_-_)
    if diffs.forall(_ == 0L) then line.last else line.last + extrapolate(diffs)

  lazy val answer1: Int = matrix.map(extrapolate).sum
  lazy val answer2: Int = matrix.map(_.reverse).map(extrapolate).sum
