package aoc2023

import nmcb.*

import scala.annotation.tailrec
import scala.math.*

object Day06 extends AoC:

  case class Race(time: Int, distance: Long):

    inline def race(speed: Long): Long = (time - speed) * speed

    lazy val wins: Int =
      @tailrec
      def loop(start: Int, end: Int, found: Int = 0): Int =
        if start <= end then
          if race(start) > distance then
            loop(start + 1, end, found + 1)
          else
            loop(start + 1, end, found)
        else
          found

      (0 to time).grouped(8).map(r => loop(start = r.start, end = r.end)).sum

  lazy val answer1: Long = List(Race(45,295L),Race(98,1734L),Race(83,1278L),Race(73,1210L)).map(_.wins).product
  lazy val answer2: Long = Race(45988373,295173412781210L).wins
