package aoc2022

import nmcb.*
import scala.io.*

object Day01 extends AoC:

  val calories: Array[Array[Int]] =
    Source
      .fromResource(s"$day.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n").map(_.toInt))


  lazy val answer1: Int = calories.map(_.sum).max
  lazy val answer2: Int = calories.map(_.sum).sorted.takeRight(3).sum
