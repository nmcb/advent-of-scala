package aoc2022

import nmcb.*

import scala.collection.mutable
import scala.io.*

object Day20 extends AoC:

  val input: List[Long] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .map(_.trim.toLong)
      .toList

  def solve(input: List[Long], key: Int = 1, runs: Int = 1): List[Long] =

    extension (buffer: mutable.Buffer[(Long,Int)]) infix def mix(element: (Long,Int)): mutable.Buffer[(Long,Int)] =
      val (steps,_) = element
      val from = buffer.indexOf(element)
      buffer.remove(from)
      val to  = (from + steps) % buffer.size
      buffer.insert(if to < 0 then to.toInt + buffer.size else to.toInt, element)
      buffer

    val indexed: List[(Long, Int)] =
      input.map(_ * key).zipWithIndex

    val decrypted: List[(Long, Int)] =
      (0 until runs)
        .foldLeft(indexed.toBuffer)((acc,_) => indexed.foldLeft(acc)(_ mix _))
        .toList

    decrypted.map((v,_) => v)

  def grove(indices: List[Int])(coords: List[Long]): List[Long] =
    def get(i: Int): Long =
      val idx = (i + coords.indexOf(0)) % coords.length
      coords(idx)
    indices.map(get)


  lazy val answer1: Long = grove(List(1000,2000,3000))(solve(input)).sum
  lazy val answer2: Long = grove(List(1000,2000,3000))(solve(input, 811589153, 10)).sum
