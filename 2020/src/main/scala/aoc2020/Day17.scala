package aoc2020

import nmcb.*
import nmcb.predef.*

object Day17 extends AoC:

  case class Cube(x: Int, y: Int, z: Int, w: Int):
    infix def +(that: Cube): Cube = Cube(x + that.x, y + that.y, z + that.z, w + that.w)

    def neighbours(offsets: Set[Cube]): Set[Cube] = offsets.map(+)

  val points: Set[Cube] =
    val sizeX = lines.head.length
    val sizeY = lines.size
    val cubes = for x <- 0 until sizeX; y <- 0 until sizeY if lines(y)(x) == '#' yield Cube(x,y,0,0)
    cubes.toSet


  def step(offsets: Set[Cube])(grid: Set[Cube]): Set[Cube] =

    def rule(cube: Cube): Boolean =
      val active = grid.contains(cube)
      val count  = cube.neighbours(offsets).count(grid.contains)
      count == 3 || (active && count == 4)

    grid.flatMap(cube => cube.neighbours(offsets)).filter(rule)

  def solve1(points: Set[Cube]): Int =
    val offsets = Seq.tabulate(3,3,3)((x,y,z) => Cube(x - 1, y - 1, z - 1, 0)).flatten.flatten.toSet
    Iterator.iterate(points)(step(offsets)).nth(6).size

  def solve2(points: Set[Cube]): Int =
    val directions = Seq.tabulate(3,3,3,3)((x,y,z,w) => Cube(x - 1, y - 1, z - 1, w - 1)).flatten.flatten.flatten.toSet
    Iterator.iterate(points)(step(directions)).nth(6).size

  lazy val answer1: Int = solve1(points)
  lazy val answer2: Int = solve2(points)
