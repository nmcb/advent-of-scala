package aoc2025

import nmcb.*

import scala.collection.immutable.NumericRange

object Day02 extends AoC:

  type Range = NumericRange[Long]

  val ranges: Vector[Range] = input.split(',').toVector.map:
    case s"$min-$max" => min.toLong to max.toLong

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

  lazy val answer1: Long = filter(ranges, doubles).sum
  lazy val answer2: Long = filter(ranges, repeats).sum
