import scala.io.Source

object Day03Marcella extends App:

  val input = Source
    .fromResource("input03.txt")
    .getLines
    .toList

  def points(item: Char): Int =
    (('a' to 'z') ++ ('A' to 'Z')).indexOf(item) + 1

  def answer1 =
    val setsItems = input.map { items => val split = items.splitAt(items.length / 2); (split._1, split._2) }
    setsItems.map(x => x._1.intersect(x._2).distinct).map(x => points(x(0))).sum

  def answer2 =
    val groupedByElves = input.grouped(3)
    groupedByElves.flatMap(x => x(0).intersect(x(1).intersect(x(2))).distinct.map(points)).sum

  println(s"Answer to part1: ${answer1}")
  println(s"Answer to part1: ${answer2}")
