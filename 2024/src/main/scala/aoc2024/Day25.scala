package aoc2024

import nmcb.*

import scala.io.*

object Day25 extends AoC:

  type Schematic = Grid[Char]

  val schematics: Vector[Schematic] =
    Source.fromResource(s"$day.txt")
      .mkString
      .split("\n\n")
      .map(_.split("\n").iterator)
      .map(Grid.fromLines).toVector

  extension (schematic: Schematic)

    def isLock: Boolean =
      schematic.row(schematic.minPos.y).forall(_ == '#')

    def isKey: Boolean =
      schematic.row(schematic.maxPos.y).forall(_ == '#')

    def heights: Vector[Int] =
      schematic
        .dropRow(if schematic.isLock then schematic.minPos.y else schematic.maxPos.y)
        .transpose
        .matrix
        .map(_.count(_ == '#'))

  def overlap(lock: Schematic, key: Schematic): Boolean =
    lock.heights.zip(key.heights).exists(_ + _ > lock.maxPos.y - 1)

  def fit(schematics: Vector[Schematic]): Int =
    val fits =
      for
        lock <- schematics.filter(_.isLock)
        key  <- schematics.filter(_.isKey)
        if !overlap(lock, key)
      yield
        (lock, key)
    fits.distinct.size

  val answer1: Int = fit(schematics)
  val answer2: Any = "<unimplemented>>"