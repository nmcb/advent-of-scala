package aoc2023

import nmcb.*

import scala.annotation.*

object Day03 extends AoC:

  case class Pos(x: Int, y: Int):
    def +(p: Pos): Pos =
      Pos(x + p.x, y + p.y)

  case class Page(chars: Vector[Vector[Char]]):

    def size: Pos =
      Pos(chars.map(_.size).max, chars.size)

    def charAt(p: Pos): Char =
      chars(p.y)(p.x)

    def isDigit(p: Pos): Boolean =
      charAt(p).isDigit

    def isSymbol(p: Pos): Boolean =
      charAt(p) != '.' && !isDigit(p)

    def isGear(c: Char): Boolean =
      c == '*'

    def isGear(p: Pos): Boolean =
      isGear(charAt(p))

    def adjacents(pos: Pos): Set[Pos] =
      Set(Pos(1,-1), Pos(1,0), Pos(1,1), Pos(0,-1), Pos(0,0), Pos(0,1), Pos(-1,-1), Pos(-1,0), Pos(-1,1))
        .map(d => pos + d)
        .filter(p => p.x >= 0 && p.x < size.x && p.y >= 0 && p.y < size.y)

    case class Num(loc: Vector[Pos], value: Int, symbols: Map[Pos,Char])

    object Num:
      def apply(loc: Vector[Pos]): Num =
        val number  = loc.map(p => charAt(p)).mkString("").toInt
        val symbols = loc.flatMap(adjacents).filter(isSymbol).map(p => p -> charAt(p)).toMap
        Num(loc, number, symbols)

    @tailrec
    private final def numbers(pos: Pos = Pos(0,0), cur: String = "", loc: Vector[Pos] = Vector.empty, acc: Vector[Num] = Vector.empty): Vector[Num] =
      if pos.y >= size.y then
        acc
      else
        if pos.x >= size.x then
          numbers(Pos(0, pos.y + 1), "", Vector.empty, if cur.nonEmpty then acc :+ Num(loc) else acc)
        else
          if isDigit(pos) then
            numbers(Pos(pos.x + 1, pos.y), s"$cur${charAt(pos)}", loc :+ pos, acc)
          else
            numbers(Pos(pos.x + 1, pos.y), "", Vector.empty, if cur.nonEmpty then acc :+ Num(loc) else acc)

    lazy val numbersWithAdjacentSymbols: Vector[Num] = numbers().filter(_.symbols.nonEmpty)

    lazy val gearsWithAdjacentNumbers: Map[Pos,Set[Num]] =
      chars
        .zipWithIndex.flatMap((l, y) => l.zipWithIndex.map((c, x) => Pos(x, y)))
        .filter(isGear)
        .map(p => p -> numbersWithAdjacentSymbols.filter(_.symbols.exists((g,_) => g == p)).toSet)
        .toMap

    lazy val gearRatios: Vector[Set[Num]] =
      val ratios =
        for
          (g1, ns1) <- gearsWithAdjacentNumbers
          (g2, ns2) <- gearsWithAdjacentNumbers
          if g1 == g2 && ns1.size == 2 && ns2.size == 2 && ns1 == ns2
        yield ns1
      ratios.toVector

  val page: Page = Page(lines.map(_.toVector).toVector)


  lazy val answer1: Int = page.numbersWithAdjacentSymbols.map(_.value).sum
  lazy val answer2: Int = page.gearRatios.map(_.map(_.value).product).sum
