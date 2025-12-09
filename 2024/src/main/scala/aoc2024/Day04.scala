package aoc2024

import nmcb.*
import pos.*

object Day04 extends AoC:

  type Direction = Pos => Pos

  val N:  Direction = p => Pos(p.x, p.y - 1)
  val E:  Direction = p => Pos(p.x + 1, p.y)
  val S:  Direction = p => Pos(p.x, p.y + 1)
  val W:  Direction = p => Pos(p.x - 1, p.y)
  val NE: Direction = p => Pos(p.x + 1, p.y - 1)
  val SE: Direction = p => Pos(p.x + 1, p.y + 1)
  val NW: Direction = p => Pos(p.x - 1, p.y - 1)
  val SW: Direction = p => Pos(p.x - 1, p.y + 1)

  val grid: Grid[Char] = Grid.fromLines(lines)
  
  extension (grid: Grid[Char])

    def read(length: Int, direction: Direction, from: Pos, result: String = ""): String =
      if length == 0 then
        result
      else
        grid.read(length - 1, direction, direction(from), result :+ grid.peekOrElse(from, '.'))

    def startsXMAS(position: Pos): Int =
      List(N, E, S, W, NE, SE, NW, SW).foldLeft(0): (count, direction) =>
        if read(4, direction, position) == "XMAS" then count + 1 else count

    def hasXMAS(position: Pos): Boolean =
      val line1 = grid.read(3, direction = SE, from = NW(position))
      val line2 = grid.read(3, direction = NE, from = SW(position))
      (line1 == "MAS" | line1 == "SAM") && (line2 == "MAS" | line2 == "SAM")


  lazy val answer1: Int = grid.positions.foldLeft(0)((count, position) => count + grid.startsXMAS(position))
  lazy val answer2: Int = grid.positions.count(hasXMAS(grid))
