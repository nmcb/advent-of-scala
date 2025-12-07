package aoc2025

import nmcb.*

import scala.annotation.*

object Day07 extends AoC:

  def solve1(manifold: Vector[String]): Int =

    @tailrec
    def loop(manifold: Vector[String], beams: Set[Int], count: Int = 0): Int =
      if manifold.nonEmpty then
        val splitters       = manifold.head.zipWithIndex.filter((s,_) => s == '^').map((_,i) => i)
        val (process, pass) = beams.partition(b => splitters.contains(b))
        val splits          = process.flatMap(b => Set(b-1, b+1))
        loop(manifold.tail, pass ++ splits, count + process.size)
      else
        count

    loop(manifold.tail, Set(manifold.head.indexOf('S')))


  extension [K] (counters: Map[K, Long])

    inline def <+>(that: Map[K, Long]): Map[K, Long] =
      that.foldLeft(counters):
        case (result, (key, count)) =>
          result.updatedWith(key):
            case None          => Some(count)
            case Some(current) => if current + count != 0 then Some(current + count) else None


  def solve2(manifold: Vector[String]): Long =

    @tailrec
    def loop(manifold: Vector[String], worlds: Map[Int, Long] = Map.empty): Long =
      if manifold.nonEmpty then
        val splitters = manifold.head.zipWithIndex.filter((s,_) => s == '^').map((_, i) => i)
        val process   = worlds.filter((w,n) => splitters.contains(w))
        val splits    = process.toSeq.flatMap((w,n) => Seq((w-1,n), (w+1,n), (w,-n))).groupMapReduce(_._1)(_._2)(_+_)
        loop(manifold.tail, worlds <+> splits)
      else
        worlds.valuesIterator.sum

    loop(manifold.tail, Map(manifold.head.indexOf('S') -> 1L))


  lazy val answer1: Long = solve1(lines)
  lazy val answer2: Long = solve2(lines)
