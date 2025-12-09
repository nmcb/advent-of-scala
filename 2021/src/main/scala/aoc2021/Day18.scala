package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day18 extends AoC:

  enum Num derives CanEqual:
    case Pair(left: Num, right: Num)
    case Lit(value: Long)

    case class Val(value: Long, depth: Int, index: Int)

    def values: List[Val] =

      def go(n: Num, d: Int): List[(Long, Int)] =
        n match
          case Pair(l, r) => go(l, d + 1) ++ go(r, d + 1)
          case Lit(value) => List((value, d - 1))

      go(this, 0).zipWithIndex.map:
        case ((v, d), i) => Val(v, d, i)

    infix def +(that: Num): Num =
      Pair(this, that).reduce

    def update(add: Long, index: Int): Num =

      def go(ix: Int, num: Num): (Int, Num) =
        num match
          case Pair(l, r) =>
            val (li, nl) = go(ix, l)
            val (ri, nr) = go(li, r)
            (ri, Pair(nl, nr))
          case Lit(v) =>
            (ix + 1, Lit(if ix == index then v + add else v))

      val (_, num) = go(0, this)

      num

    def magnitude: Long =
      this match
        case Lit(v)     => v
        case Pair(l, r) => 3 * l.magnitude + 2 * r.magnitude

    @tailrec
    final def reduce: Num =
      explode.orElse(split) match
        case None      => this
        case Some(num) => num.reduce

    def explode: Option[Num] =
      values.filter(_.depth == 4).take(2) match
        case List(Val(l, _, li), Val(r, _, ri)) => Some(addl(l, li).addr(r, ri).fix(li))
        case _                                  => None

    def addl(value: Long, index: Int): Num =
      values.takeWhile(_.index < index).lastOption match
        case Some(Val(_, _, i)) => update(value, i)
        case None               => this

    def addr(value: Long, index: Int): Num =
      values.dropWhile(_.index <= index).headOption match
        case Some(Val(_, _, i)) => update(value, i)
        case None               => this

    def fix(index: Int): Num =

      def go(ix: Int, depth: Int, num: Num): (Int, Num) =
        num match
          case Pair(l, r) =>
            val (li, nl) = go(ix, depth + 1, l)
            val (ri, nr) = go(li, depth + 1, r)
            (ri, if ix == index && depth == 4 then Lit(0) else Pair(nl, nr))
          case Lit(v) => (ix + 1, Lit(v))

      val (_, num) = go(0, 0, this)

      num

    def split: Option[Num] =
      this match
        case Lit(v) if v >= 10 => Some(Pair(Lit(v / 2), Lit(v / 2 + v % 2)))
        case Lit(_)            => None
        case Pair(l, r)        => (l.split, r.split) match
                                    case (None, None)    => None
                                    case (Some(s), _)    => Some(Pair(s, r))
                                    case (None, Some(s)) => Some(Pair(l, s))

  import Num.*

  def parse(s: String): Num =
    import P.*
    def vp: P[Num] = digits.map(Lit.apply)
    def pp: P[Num] = for { _ <- char('[') ; l <- np ; _ <- char(',') ; r <- np ; _ <- char(']') } yield Pair(l, r)
    def np: P[Num] = vp | pp
    run(np)(s)  

  val numbers: Vector[Num] = lines.map(parse)


  lazy val answer1: Long = numbers.reduceLeft(_ + _).magnitude
  lazy val answer2: Long = numbers.combinations(2).flatMap(ns => List(ns(0) + ns(1), ns(1) + ns(0))).map(_.magnitude).max
