package marcella

import scala.io.Source

object Day03Marcella extends App :

  val input: List[String] =
    Source
      .fromResource("input03.txt")
      .getLines
      .toList

  def points(item: Char): Int =
    (('a' to 'z') ++ ('A' to 'Z')).indexOf(item) + 1

  val answer1 =
    input
      .map(items => items.splitAt(items.length / 2))
      .map((l, r) => l.intersect(r).distinct)
      .map(x => points(x(0)))
      .sum

  println(s"Answer to part1: ${answer1}")

  val answer2 =
    input
      .grouped(3)
      .flatMap(x => x(0).intersect(x(1).intersect(x(2))).distinct.map(points))
      .sum

  println(s"Answer to part1: ${answer2}")
