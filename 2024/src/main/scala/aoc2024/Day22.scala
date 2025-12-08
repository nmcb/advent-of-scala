package aoc2024

import nmcb.*
import nmcb.predef.*

import scala.collection.*

object Day22 extends AoC:

  val numbers: Vector[Long] = lines.map(_.toLong).toVector

  inline def mix(l: Long)(n: Long): Long =
    l ^ n

  inline def prune(n: Long): Long =
    n % 16777216

  inline def mul(v: Long): Long => Long =
    phases(_ * v)

  inline def div(v: Long): Long => Long =
    phases(_ / v)

  inline def phases(operation: Long => Long): Long => Long =
    (n: Long) => (operation andThen mix(n) andThen prune)(n)

  inline def stages(number: Long): Long =
    (mul(64) andThen div(32) andThen mul(2048))(number)

  extension (n: Long)
    def nextSecret: Long =
      stages(n)

  extension (p: (Seq[Long],Long))
    def sequence: Seq[Long] = p._1
    def price: Long         = p._2

  def solve(secrets: Vector[Long]): Long =
    secrets
      .flatMap: initial =>
        Iterator.iterate(initial, 2001)(_.nextSecret)
          .map(_ % 10)
          .sliding(5)
          .map: prices =>
            prices
              .zip(prices.tail)
              .map(-_ + _)
              -> prices.last
          .toVector
          .groupMap(_.sequence)(_.price)
          .map(_ -> _.head)
      .groupMapReduce(_.sequence)(_.price)(_ + _)
      .values
      .max

  lazy val answer1: Long = numbers.map(initial => Iterator.iterate(initial, 2001)(_.nextSecret).drain).sum
  lazy val answer2: Long = solve(numbers)
