package aoc2020

import nmcb.*

object Day05 extends AoC:

  type Id = Int

  import Integer.*

  def seatId(s: String): Id =
    val row = parseInt(s.substring(0, 7).replaceAll("F","0").replaceAll("B","1"), 2)
    val col = parseInt(s.substring(7,10).replaceAll("L","0").replaceAll("R","1"), 2)
    row * 8 + col

  val seatIds: Set[Id] = lines.map(seatId).toSet

  def missing(seatIds: Set[Id]): Set[Id] =
    (seatIds.min to seatIds.max).toSet -- seatIds

  lazy val answer1: Id = seatIds.max
  lazy val answer2: Id = missing(seatIds).head
