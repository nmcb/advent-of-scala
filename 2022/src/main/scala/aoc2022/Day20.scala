package aoc2022

import nmcb.*

import scala.collection.mutable

object Day20 extends AoC:

  val numbers: Vector[Long] = lines.map(_.trim.toLong)

  def solve(input: Vector[Long], key: Int = 1, runs: Int = 1): Vector[Long] =

    extension (buffer: mutable.Buffer[(Long,Int)]) infix def mix(element: (Long,Int)): mutable.Buffer[(Long,Int)] =
      val (steps,_) = element
      val from = buffer.indexOf(element)
      buffer.remove(from)
      val to  = (from + steps) % buffer.size
      buffer.insert(if to < 0 then to.toInt + buffer.size else to.toInt, element)
      buffer

    val indexed: Vector[(Long, Int)] =
      input.map(_ * key).zipWithIndex

    val decrypted: Vector[(Long, Int)] =
      (0 until runs)
        .foldLeft(indexed.toBuffer)((acc,_) => indexed.foldLeft(acc)(_ mix _))
        .toVector

    decrypted.map((v,_) => v)

  def grove(indices: Vector[Int])(coords: Vector[Long]): Vector[Long] =
    def get(i: Int): Long =
      val idx = (i + coords.indexOf(0)) % coords.length
      coords(idx)
    indices.map(get)


  lazy val answer1: Long = grove(Vector(1000,2000,3000))(solve(numbers)).sum
  lazy val answer2: Long = grove(Vector(1000,2000,3000))(solve(numbers, 811589153, 10)).sum
