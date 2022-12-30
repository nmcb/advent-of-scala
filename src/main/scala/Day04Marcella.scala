import scala.io.Source

object Day04Marcella extends App:

  val input = Source
    .fromResource("input04.txt")
    .getLines
    .map {case s"${r1}-${r2},${r3}-${r4}" => ((r1.toInt to r2.toInt), (r3.toInt to r4.toInt))}
    .toList

  def checkFullOverlap(r1: Range, r2: Range): Boolean =
    val overlap = r1.intersect(r2)
    overlap == r1 | overlap == r2

  def checkOverlap(r1: Range, r2: Range): Boolean =
    r1.intersect(r2).nonEmpty

  val answer1 = input.map(checkFullOverlap).count(_ == true)
  println(s"Answer to part 1: ${answer1}")

  val answer2 = input.map(checkOverlap).count(_ == true)
  println(s"Answer to part 2: ${answer2}")
