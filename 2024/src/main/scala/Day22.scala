import nmcb.predef.*

import scala.collection.*
import scala.io.*

object Day22 extends App:

  val day: String           = getClass.getName.filter(_.isDigit).mkString("")
  val input: Vector[Long] = Source.fromResource(s"input$day.txt").getLines.map(_.toLong).toVector

  inline def mix(l: Long)(n: Long) =
    l ^ n

  inline def prune(n: Long) =
    n % 16777216

  inline def mul(v: Long) =
    phases(_ * v)

  inline def div(v: Long) =
    phases(_ / v)

  inline def phases(operation: Long => Long) =
    (n: Long) => (operation andThen mix(n) andThen prune)(n)

  inline def stages(number: Long) =
    (mul(64) andThen div(32) andThen mul(2048))(number)

  extension (n: Long)
    def nextSecret: Long =
      stages(n)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.map(initial => Iterator.iterate(initial, 2001)(_.nextSecret).drain).sum
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

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

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve(input)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
