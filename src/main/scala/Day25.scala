import nmcb.*

import scala.io.*

object Day25 extends App:

  val day: String = getClass.getName.filter(_.isDigit).mkString("")

  val schematics: Vector[Grid[Char]] =
    Source.fromResource(s"input$day.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n").iterator)
      .map(Grid.fromLines).toVector

  extension (schema: Grid[Char])

    def isLock: Boolean =
      schema.row(schema.minPos.y).forall(_ == '#')

    def isKey: Boolean =
      schema.row(schema.maxPos.y).forall(_ == '#')

    def heights: Vector[Int] =
      schema
        .dropRow(if schema.isLock then schema.minPos.y else schema.maxPos.y)
        .transpose
        .matrix
        .map(_.count(_ == '#'))

  def overlap(lock: Grid[Char], key: Grid[Char]): Boolean =
    lock.heights.zip(key.heights).exists(_ + _ > lock.maxPos.y - 1)

  def fit(schematics: Vector[Grid[Char]]): Int =
    val fits =
      for
        lock <- schematics.filter(_.isLock)
        key  <- schematics.filter(_.isKey)
        if !overlap(lock, key)
      yield
        (lock, key)

    fits.distinct.size

  val start1: Long = System.currentTimeMillis
  val answer1: Int = fit(schematics)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
