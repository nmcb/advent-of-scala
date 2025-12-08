package aoc2023

import nmcb.*

import scala.collection.*
import scala.collection.immutable.Seq

object Day12 extends AoC:

  case class Line(mask: String, lengths: List[Int]):
    def unfold: Line =
      Line(Seq.fill(5)(mask).reduce(_ ++ "?" ++ _), Seq.fill(5)(lengths).reduce(_ ++ _))

  object Line:
    def fromString(s: String): Line =
      val Array(mask, groups) = s.split(' ')
      Line(mask.trim, groups.trim.split(',').map(_.toInt).toList)

  case class Puzzle(lines: List[Line]):

    val cache = mutable.Map.empty[(String, List[Int]), Long]

    private def solve(mask: String, lengths: List[Int]): Long =
      cache.getOrElseUpdate((mask, lengths),
        (mask, lengths) match
          case (_, Nil) if mask.contains('#') => 0
          case (_, Nil)                       => 1
          case ("", _ :: _)                   => 0
          case (s".$tail", _)                 => solve(tail, lengths)
          case (s"#$tail", length :: todo) if mask.length >= length =>
            val (operational, rest) = tail.splitAt(length - 1)
            if operational.contains('.') then
              0
            else
              rest match
                case ""      => solve("", todo)
                case s".$mt" => solve(mt, todo)
                case s"?$mt" => solve(mt, todo)
                case _       => 0
          case (s"#$tail", _ :: _) => 0
          case (s"?$tail", _)      => solve(s".$tail", lengths) + solve(s"#$tail", lengths)
          case _                   => sys.error(s"illegal state: mask=$mask, lengths=$lengths")
      )

    lazy val arrangements: Long =
      lines.map(l => solve(l.mask, l.lengths)).sum

    lazy val unfoldAll: Puzzle =
      Puzzle(lines.map(_.unfold))

  lazy val puzzle: Puzzle = Puzzle(lines.map(Line.fromString).toList)


  lazy val answer1: Long = puzzle.arrangements
  lazy val answer2: Long = puzzle.unfoldAll.arrangements
