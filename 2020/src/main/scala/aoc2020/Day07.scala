package aoc2020

import nmcb.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day07 extends AoC:

  val Line: Regex = """(\w+)\s(\w+)\sbags\scontain\s(.+).""".r
  val Bags: Regex = """(\d+)\s(\w+)\s(\w+)\sbag[s]?""".r

  type Color = String
  type Bags  = Vector[Bag]
  case class Bag(from: Color, to: Option[Color], count: Int)

  val bags: Bags =
    lines.flatMap:
        case Line(from1, from2, contains) => contains.split(',').map(_.trim).map:
          case Bags(count, to1, to2) => Bag(from1 + from2, Some(to1 + to2), count.toInt)
          case _                     => Bag(from1 + from2, None, 0)

  extension (bags: Bags)

    def parentsOf(to: Color): Vector[Color] =
      bags.filter(b => b.to.contains(to)).map(_.from)

    @tailrec
    def solve1(ps: Vector[Color], result: Vector[Color] = Vector.empty): Vector[Color] =
      if ps.isEmpty then
        result
      else
        val end = ps.filter(p => parentsOf(p).isEmpty)
        val rec = ps.filter(p => parentsOf(p).nonEmpty)
        solve1(rec.flatMap(p => parentsOf(p)).distinct, result ++ rec ++ end)

    def childrenOf(from: Color): Vector[(Color,Int)] =
      bags.filter(b => b.from == from && b.to.isDefined).map(b => b.to.get -> b.count)

    def solve2(inner: Vector[(Color,Int)], result: Int = 0): Int =
        inner match
          case Vector()            => result
          case (child,count) +: cs => solve2(cs, result + count + count * solve2(childrenOf(child)))
          case _                   => sys.error("boom!")


  lazy val answer1: Int = bags.solve1(bags.parentsOf("shinygold")).distinct.size
  lazy val answer2: Int = bags.solve2(bags.childrenOf("shinygold"))
