package marcella

import scala.io.Source

object Day06Marcella extends App :

  val input: List[String] =
    Source.fromResource("input06.txt").getLines.toList

  def findStart(inputList: List[String], window: Int) =
    inputList
      .map(_.sliding(window)
        .zipWithIndex.filter { case (char, index) => (char.distinct.size == window) }
        .map { case (char, index) => (index + window) }
        .min)
      .sum

  println("Answer to part 1: " + findStart(input, 4))
  println("Answer to part 2: " + findStart(input, 14))
