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

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = input.map(initial => Iterator.range(0, 2000).foldLeft(initial)((l,_) => l.next)).map(_.number).sum

  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  case class Buyer(initial: Secret):
    val secrets = Vector.range(0, 1999).foldLeft(Vector(initial))((a,_) => a :+ a.last.next)
    val prices  = secrets.map(_.number.toString.last.asDigit)
    val diffs   = 0 +: prices.sliding(2).map(d => d(1) - d(0)).toVector

    val highestPrice          = prices.drop(5).max
    val highestPriceIndexes   = prices.zipWithIndex.drop(5).filter(_._1 == highestPrice).map(_._2)

    val priceWithIndexToPriceSequence =
      (0 to 9).foldLeft(Vector.empty[((Int,Int),Vector[Int])]): (acc,price) =>
        val priceIndexes = prices.zipWithIndex.drop(5).filter(_._1 == price).map(_._2)

        priceIndexes.foldLeft(acc): (acc,idx) =>
          acc :+ (prices(idx) -> idx) -> diffs.slice(idx - 3, idx + 1)

    def pricesWithIndexesBy(sequence: Vector[Int]): Vector[(Int,Int)] =
      diffs.sliding(4)
        .zipWithIndex
        .map((seq,idx) => prices(idx + 3) -> (idx + 3) -> seq)
        .filter(_._2 == sequence)
        .map(_._1)
        .toVector


  val buyers = input.map(Buyer.apply)
//  println(buyers.map(_.diffs.mkString("\n")))
//  println(buyers.map(_.highestPrice))
//  println(buyers.map(_.highestPriceIndexes))


  val x = buyers.map(_.pricesWithIndexesBy(Vector(-2,1,-1,3)))
  println(x)

  val y = buyers.map(b => b.priceWithIndexToPriceSequence.)
//  println(y)




  val start2: Long = System.currentTimeMillis
  val answer2: Long = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
