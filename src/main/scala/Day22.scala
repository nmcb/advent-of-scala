import nmcb.predef.*

import scala.collection.*
import scala.io.*

object Day22 extends App:

  val day: String           = getClass.getName.filter(_.isDigit).mkString("")
  val input: Vector[Secret] = Source.fromResource(s"input$day.txt").getLines.map(_.toLong).map(Secret.apply).toVector

  case class Secret(number: Long):

    inline def prune(n: Long): Long  = n % 16777216
    inline def mix(l: Long, n: Long) = l ^ n

    inline def mul(v: Long, n: Long): Long =
      val mul    = n * v
      val mixed  = mix(mul, n)
      val pruned = prune(mixed)
      pruned

    inline def div(v: Long, n: Long): Long =
      val div    = n / v
      val mixed  = mix(div, n)
      val pruned = prune(mixed)
      pruned

    def next: Secret =
      val stage1 = mul(64, number)
      val stage2 = div(32, stage1)
      val stage3 = mul(2048, stage2)

      Secret(stage3)

  extension (l: Long)
    def bin: String = l.toBinaryString.padTo(64 - l.toString.length, '0')






  println(input.mkString("\n"))

  val start1: Long = System.currentTimeMillis
  val answer1: Long =
    input
      .map: initial =>
        val n =
          Iterator.range(0, 2000)
          .foldLeft(initial): (l, _) =>
            val n = l.next
            n
          .number
        println(n)
        n
      .sum

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Long = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
