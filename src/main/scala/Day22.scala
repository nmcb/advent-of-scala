import nmcb.predef.*

import scala.collection.*
import scala.io.*

object Day22 extends App:

  val day: String           = getClass.getName.filter(_.isDigit).mkString("")
  val input: Vector[Secret] = Source.fromResource(s"input$day.txt").getLines.map(_.toLong).map(Secret.apply).toVector

  case class Secret(number: Long):

    inline def mix(l: Long)(n: Long) =
      l ^ n

    inline def prune(n: Long): Long =
      n % 16777216

    inline def phases(operation: Long => Long)(n: Long): Long =
      (operation andThen mix(n) andThen prune)(n)

    inline def mul(v: Long)(n: Long): Long =
      phases(_ * v)(n)

    inline def div(v: Long)(n: Long): Long =
      phases(_ / v)(n)

    inline def stages(number: Long): Long =
      (mul(64) andThen div(32) andThen mul(2048))(number)

    def next: Secret =
      Secret(stages(number))

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.map[Long](initial => Iterator.iterate(initial, 2001)(_.next).map(_.number).drain).sum
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  extension (p: (Seq[Long],Long))
    def sequence: Seq[Long] = p._1
    def price: Long         = p._2

  def solve(secrets: Vector[Secret]): Long =
    secrets
      .flatMap: initial =>
        Iterator.iterate(initial, 2001)(_.next)
          .map(_.number % 10)
          .sliding(5)
          .map: prices =>
            prices
              .zip(prices.tail)
              .map((a,b) => b - a)
              -> prices.last
          .toVector
          .groupMap(_.sequence)(_.price)
          .map(_ -> _.head)
      .groupMapReduce(_.sequence)(_.price)(_ + _)
      .values
      .max

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = solve(input)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
