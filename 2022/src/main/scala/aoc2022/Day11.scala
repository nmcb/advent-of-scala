package aoc2022

import nmcb.*

import scala.io.*
import scala.util.*

object Day11 extends AoC:

  lazy val monkeys: List[Monkey] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .filterNot(_.isBlank)
      .grouped(6)
      .map(_.toList)
      .map(Monkey.fromStrings)
      .toList

  object Monkey:
    def fromStrings(ss: List[String]): Monkey =
      def parseMonkey(s: String): Int =
        s match
          case s"Monkey $nr:" => nr.toInt
      def parseItems(s: String): List[Long] =
        s match
          case s"  Starting items: $is" => is.split(',').map(n => n.trim.toLong).toList
      def parseOperation(s: String): (String,String) =
        s match
          case s"  Operation: new = old $op $r" => (op,r)
      def parseTest(s: String): Long =
        s match
          case s"  Test: divisible by $d" => d.toLong
      def parseToIfTrue(s: String): Int =
        s match
          case s"    If true: throw to monkey $m" => m.toInt
      def parseToIfFalse(s: String): Int =
        s match
          case s"    If false: throw to monkey $m" => m.toInt

      val nr        = parseMonkey(ss(0))
      val items     = parseItems(ss(1))
      val (op,rhs)  = parseOperation(ss(2))
      val divisible = parseTest(ss(3))
      val tit       = parseToIfTrue(ss(4))
      val tif       = parseToIfFalse(ss(5))

      Monkey(nr, items, op, Try(rhs.toLong).toOption, divisible, tit, tif)


  case class Monkey( nr: Int
                   , items: List[Long]
                   , op: String
                   , rhs: Option[Long]
                   , divisible: Long
                   , toIfTrue: Int
                   , toIfFalse: Int
                   , lowerWorry: Long => Long = identity
                   , count: Long              = 0
  ):
    def operation(i: Long): Long =
      op match
        case "+" => i + rhs.getOrElse(i)
        case "*" => i * rhs.getOrElse(i)

    private def inspect(worry0: Long): (Long,Int) =
      val worry1 = operation(worry0)
      val worry2 = lowerWorry(worry1)
      val nr = if worry2 % divisible == 0 then toIfTrue else toIfFalse
      (worry2, nr)

    def inspect: List[(Long,Int)] =
      items.map(inspect)

  def round(ms: List[Monkey]): List[Monkey] =
    def loop(ms: List[Monkey], acc: List[Monkey]): List[Monkey] =
      ms match
        case Nil => acc
        case m :: t =>
          val us  = acc(m.nr).inspect
          val ums = us.foldLeft(acc) { case (s, (i, nr)) =>
            s.updated(nr, s(nr).copy(items = s(nr).items :+ i))
          }
          val nms = ums.updated(m.nr, ums(m.nr).copy(items = List.empty, count = m.count + us.size))
          loop(t, nms)
    loop(ms, ms)

  def solve(rounds: Int, ms: List[Monkey]): Long =
    (1 to rounds)
      .foldLeft(ms)((s,_) => round(s))
      .map(_.count)
      .sorted
      .takeRight(2)
      .product


  lazy val answer1: Long =
    val ms = monkeys.map(_.copy(lowerWorry = _ / 3))
    solve(20, monkeys.map(_.copy(lowerWorry = _ / 3)))

  lazy val answer2: Long =
    val productOfDivisible = monkeys.map(_.divisible).product
    val ms = monkeys.map(_.copy(lowerWorry = _ % productOfDivisible))
    solve(10000, ms)
