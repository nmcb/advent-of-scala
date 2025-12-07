package aoc2023

import nmcb.*

import scala.io.*

object Day09 extends AoC:

  lazy val input: Vector[Vector[Int]] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .map(_.split(' ').map(_.toInt).toVector)
      .toVector

  def extrapolate(line: Vector[Int]): Int =
    val diffs = line.tail.zip(line).map(_-_)
    if diffs.forall(_ == 0L) then line.last else line.last + extrapolate(diffs)

  lazy val answer1: Int = input.map(extrapolate).sum
  lazy val answer2: Int = input.map(_.reverse).map(extrapolate).sum
