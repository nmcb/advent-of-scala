package aoc2025

import nmcb.*

import scala.collection.immutable.NumericRange
import scala.io.*

object Day02 extends AoC:

  type Range = NumericRange[Long]

  val input: Vector[Range] =
      Source
        .fromResource(s"$day.txt")
        .mkString
        .trim
        .split(',')
        .map:
          case s"$min-$max" => min.toLong to max.toLong
        .toVector

  def doubles(id: Long): Boolean =
    val s = id.toString
    s.length % 2 == 0 && (s.drop(s.length / 2) == s.dropRight(s.length / 2))

  def filter(input: Vector[Range], invalid: Long => Boolean): Vector[Long] =
    for
      range <- input
      id    <- range
      if invalid(id)
    yield
      id

  def repeats(id: Long): Boolean =
    val s = id.toString
    val doubled = s + s
    doubled.substring(1, doubled.length - 1).contains(s)

  val answer1: Long = filter(input, doubles).sum
  val answer2: Long = filter(input, repeats).sum
