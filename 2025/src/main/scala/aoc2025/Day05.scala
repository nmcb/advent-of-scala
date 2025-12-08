package aoc2025

import nmcb.*

object Day05 extends AoC:

  type Range = (min: Long, max: Long)

  val (ranges: Vector[Range], ingredients: Vector[Long]) =
    val Array(top, bottom) = input.split("\n\n")

    val rs = top.linesIterator
      .map:
        case s"$min-$max" => (min = min.toLong, max = max.toLong)
      .toVector

    val is = bottom.linesIterator.map(_.toLong).toVector

    (rs, is)


  extension (range: Range)

    inline def contains(l: Long): Boolean =
      l >= range.min && l <= range.max

    inline def size: Long =
      range.max - range.min + 1

  extension (ranges: Vector[Range])

    def merge: Vector[Range] =
      ranges.sortBy(_.min).foldLeft(Vector.empty[Range]):
        case (tail :+ last, test) if test.min <= last.max + 1 => tail :+ (last.min, last.max max test.max)
        case (result, range)                                  => result :+ range

  lazy val answer1: Int = ingredients.count(i => ranges.exists(_.contains(i)))
  lazy val answer2: Long = ranges.merge.map(_.size).sum
