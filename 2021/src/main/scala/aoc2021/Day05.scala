package aoc2021

import nmcb.*

import scala.collection.*
import scala.collection.immutable.{Map, Set}
import scala.util.matching.Regex

object Day05 extends AoC:

  val LineLit: Regex = """(\d+),(\d+) -> (\d+),(\d+)""".r

  val puzzle: Vector[Line] =
    lines
      .map:
        case LineLit(x0, y0, x1, y1) if y0 == y1 =>
          HLine(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case LineLit(x0, y0, x1, y1) if x0 == x1 =>
          VLine(x0.toInt, y0.toInt, x1.toInt, y1.toInt)
        case LineLit(x0, y0, x1, y1) =>
          DLine(x0.toInt, y0.toInt, x1.toInt, y1.toInt)

  sealed trait Line:
    val points: Set[(Int,Int)]

  case class HLine(x0: Int, y0: Int, x1: Int, y1: Int) extends Line:

    val points: Set[(Int,Int)] =
      if x0 < x1 then
        (x0 to x1).map((_,y0)).toSet
      else
        (x1 to x0).map((_,y0)).toSet

  case class VLine(x0: Int, y0: Int, x1: Int, y1: Int) extends Line:

    val points: Set[(Int,Int)] =
      if y0 < y1 then
        (y0 to y1).map((x0, _)).toSet
      else
        (y1 to y0).map((x0, _)).toSet

  case class DLine(x0: Int, y0: Int, x1: Int, y1: Int) extends Line:

    val points: Set[(Int,Int)] =
      (0 to (math.max(x0, x1) - math.min(x0, x1)))
        .map: n =>
          if x0 < x1 then
            if y0 < y1 then
              (x0 + n, y0 + n)
            else
              (x0 + n, y0 - n)
          else
            if y0 < y1 then
              (x0 - n, y0 + n)
            else
              (x0 - n, y0 - n)
        .toSet

  case class Floor(locations: Map[(Int,Int), Int]):

    val crossings: Int =
      locations.count((_,nr) => nr >= 2)

  object Floor:

    def apply(lines: Vector[Line]): Floor =
        val locations = mutable.Map.empty[(Int,Int),Int]
        for l <- lines ; p <- l.points do
          locations.updateWith(p):
            case None     => Some(1)
            case Some(nr) => Some(nr + 1)
        Floor(locations.toMap)


  lazy val answer1: Int = Floor(puzzle.filterNot(_.isInstanceOf[DLine])).crossings
  lazy val answer2: Int = Floor(puzzle).crossings
