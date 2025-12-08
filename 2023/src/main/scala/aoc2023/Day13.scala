package aoc2023

import nmcb.*

object Day13 extends AoC:

  case class Image(image: Vector[Vector[Char]], defects: Int = 0):

    def smudgeCorrection(on: Boolean): Image =
      copy(defects = if on then 1 else 0)

    private lazy val column: Int =
      (1 until image.last.size).find: n =>
        image.count: line =>
          val (l, r) = line splitAt n
          val length = l.length min r.length
          l.reverse.take(length) != r.take(length)
        == defects
      .getOrElse(0)

    private lazy val row: Int =
      copy(image.transpose).column

    lazy val summarize: Int =
      column + (100 * row)

  object Image:
    def fromString(ss: Array[String]): Image =
      Image(ss.map(_.toVector).toVector)

  lazy val images: Vector[Image] =
    input
      .split("\n\n")
      .map(_.split("\n"))
      .map(Image.fromString)
      .toVector

  lazy val answer1: Long = images.map(_.summarize).sum
  lazy val answer2: Long = images.map(_.smudgeCorrection(true).summarize).sum
