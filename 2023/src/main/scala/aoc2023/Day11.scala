package aoc2023

import nmcb.*
object Day11 extends AoC:

  lazy val image: Image = Image(lines.map(_.toVector))

  case class Pos(x: Int, y: Int)

  case class Image(galaxies: Vector[Vector[Char]]):

    lazy val positions: Vector[Pos] =
      galaxies.zipWithIndex.flatMap((l,y) =>
        l.zipWithIndex.flatMap((c,x) =>
          Option.when(c == '#')(Pos(x, y))))

    def empty(gs: Vector[Vector[Char]]): Set[Int] =
      gs.zipWithIndex.flatMap((line,i) => if line.forall(_ == '.') then Some(i) else None).toSet

    lazy val emptyY: Set[Int] =
      empty(galaxies)

    lazy val emptyX: Set[Int] =
      empty(galaxies.transpose)

    def expand(a: Int, b: Int, indices: Set[Int], multiplier: Long = 1L): Long =
      val range = if a <= b then a until b else b until a
      range.map(n => if indices.contains(n) then multiplier else 1L).sum

    def distances(multiplier: Long): Vector[Long] =
      positions.zipWithIndex.flatMap((g0,n) =>
        positions.drop(n).map(g1 =>
          expand(g0.x, g1.x, emptyX, multiplier) + expand(g0.y, g1.y, emptyY, multiplier)))

    override def toString: String =
      galaxies.map(_.mkString("")).mkString("", "\n", "\n")


  lazy val answer1: Long = image.distances(2L).sum
  lazy val answer2: Long = image.distances(1000000L).sum
