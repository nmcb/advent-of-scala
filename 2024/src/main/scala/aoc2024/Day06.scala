package aoc2024

import nmcb.*
import pos.*

import scala.annotation.*

object Day06 extends AoC:

  import nmcb.Dir.*

  private val grid: Grid[Char] = Grid.fromLines(lines)

  extension (g: Grid[Char])

    @tailrec
    def walkGuard(pos: Pos, dir: Dir, result: Set[Pos] = Set.empty): Set[Pos] =
      val next = pos step dir
      g.peekOrElse(next, ' ') match
        case ' '       => result + pos
        case '.' | '^' => walkGuard(next, dir, result + pos)
        case '#'       => walkGuard(pos, dir.ccw, result)

    def peekWithObstruction(p: Pos, obstruct: Pos): Char =
        if p == obstruct then '#' else g.peekOrElse(p, ' ')

    @tailrec
    def walkCircular(pos: Pos, dir: Dir, obstruct: Pos, visited: Set[(Pos,Dir)] = Set.empty): Boolean =
      if visited.contains((pos, dir)) then
        true
      else
        val next = pos step dir
        g.peekWithObstruction(next, obstruct) match
          case ' '       => false
          case '.' | '^' => walkCircular(next, dir, obstruct, visited + (pos -> dir))
          case '#'       => walkCircular(pos, dir.cw, obstruct, visited)


  def start: Pos   = grid.findOne('^')
  lazy val answer1: Int = grid.walkGuard(start, N).size

  def obstructions: Set[Pos] = grid.walkGuard(start, N) - start
  lazy val answer2: Int = obstructions.count(o => grid.walkCircular(start, N, o))
