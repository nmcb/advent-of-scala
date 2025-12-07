package aoc2025

import nmcb.*

import scala.annotation.*
import scala.io.*

object Day07 extends AoC:

  val manifold: Vector[String] = Source.fromResource(s"$day.txt").getLines.toVector

  def solve1(manifold: Vector[String]): Int =

    @tailrec
    def loop(manifold: Vector[String], beams: Set[Int], count: Int = 0): Int =
      if manifold.nonEmpty then
        val splitters       = manifold.head.zipWithIndex.filter((s,_) => s == '^').map((_,i) => i)
        val (process, pass) = beams.partition(b => splitters.contains(b))
        val split           = process.flatMap(b => Set(b-1, b+1))
        loop(manifold.tail, pass ++ split, count + process.size)
      else
        count

    loop(manifold.tail, Set(manifold.head.indexOf('S')))


  extension (counter: Map[Int, Long])

    inline def <+>(that: Map[Int, Long]): Map[Int, Long] =
      that.foldLeft(counter):
        case (result, (b, n)) =>
          result.updatedWith(b):
            case None          => Some(n)
            case Some(current) => if current + n != 0 then Some(current + n) else None


  def solve2(manifold: Vector[String]): Long =

    @tailrec
    def loop(manifold: Vector[String], worlds: Map[Int, Long] = Map.empty): Long =
      if manifold.nonEmpty then
        val splitters = manifold.head.zipWithIndex.filter((s,_) => s == '^').map((_, i) => i)
        val process   = worlds.filter((w,n) => splitters.contains(w))
        val split     = process.toSeq.flatMap((w,n) => Seq((w-1,n), (w+1,n), (w,-n))).groupMapReduce(_._1)(_._2)(_+_)
        loop(manifold.tail, worlds <+> split)
      else
        worlds.valuesIterator.sum

    loop(manifold.tail, Map(manifold.head.indexOf('S') -> 1L))


  lazy val answer1: Long = solve1(manifold)
  lazy val answer2: Long = solve2(manifold)
