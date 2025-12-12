package aoc2025

import nmcb.*

object Day12 extends AoC:

  case class Region(width: Int, height: Int, amounts: Vector[Int]):

    val volume: Int = width * height

  case class Present(pattern: Vector[Vector[Char]]):

    val volume: Int =
      pattern.map(_.count(_ == '#')).sum

  extension (presents: Vector[Present])

    def volume(amounts: Vector[Int]): Int =
      amounts
        .zip(presents)
        .map((amount, present) => amount * present.volume)
        .sum

  def solve1(regions: Vector[Region], presents: Vector[Present]): Int =
    regions.count(region => presents.volume(region.amounts) <= region.volume)

  lazy val regions: Vector[Region] = chunks.last.collect:
    case s"${w}x${h}: $nums" =>
      Region(
        width = w.toInt,
        height = h.toInt,
        amounts = nums.split(" ").map(_.toInt).toVector,
      )

  lazy val presents: Vector[Present] = chunks.init
    .map: lines =>
      Present(lines.tail.map(_.toVector))


  override lazy val answer1: Int    = solve1(regions, presents)
  override lazy val answer2: String = "<unimplemented>"
