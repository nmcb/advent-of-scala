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

  val answer1 = findStart(input, 4)
  val answer2 = findStart(input, 14)

  println(s"Answer to part 1: $answer1")
  println(s"Answer to part 2: $answer2")
