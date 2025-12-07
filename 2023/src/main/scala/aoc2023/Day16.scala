package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day16 extends AoC:

  enum Dir(val step: Pos):
    case N extends Dir(Pos( 0,-1))
    case E extends Dir(Pos( 1, 0))
    case S extends Dir(Pos( 0, 1))
    case W extends Dir(Pos(-1, 0))

  import Dir.*

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y)

  case class Grid(grid: Vector[Vector[Char]]):
    private val sizeX: Int = grid.map(_.size).max
    private val sizeY: Int = grid.size

    private def tile(p: Pos): Char =
      grid(p.y)(p.x)

    private def advance(p: Pos, d: Dir): List[(Pos,Dir)] =
      val result: List[(Pos,Dir)] =
        (tile(p), d) match
          case ('.', N)  => List(p + N.step -> N)
          case ('.', E)  => List(p + E.step -> E)
          case ('.', S)  => List(p + S.step -> S)
          case ('.', W)  => List(p + W.step -> W)

          case ('|', N)  => List(p + N.step -> N)
          case ('|', E)  => List(p + N.step -> N, p + S.step -> S)
          case ('|', S)  => List(p + S.step -> S)
          case ('|', W)  => List(p + N.step -> N, p + S.step -> S)

          case ('-', N)  => List(p + W.step -> W, p + E.step -> E)
          case ('-', E)  => List(p + E.step -> E)
          case ('-', S)  => List(p + W.step -> W, p + E.step -> E)
          case ('-', W)  => List(p + W.step -> W)

          case ('/', N)  => List(p + E.step -> E)
          case ('/', E)  => List(p + N.step -> N)
          case ('/', S)  => List(p + W.step -> W)
          case ('/', W)  => List(p + S.step -> S)

          case ('\\', N) => List(p + W.step -> W)
          case ('\\', E) => List(p + S.step -> S)
          case ('\\', S) => List(p + E.step -> E)
          case ('\\', W) => List(p + N.step -> N)

          case _ => sys.error(s"advancing: p=$p, d=$d")

      result.filter((p,_) => p.x >= 0 && p.y >= 0 && p.x < sizeX && p.y < sizeY)

    private def trace(start: Pos, direction: Dir): Set[(Pos,Dir)] =
      @tailrec
      def loop(todo: List[(Pos,Dir)], acc: Set[(Pos,Dir)] = Set.empty): Set[(Pos,Dir)] =
        todo match
          case Nil                                  => acc
          case (p,d) :: rest if acc.contains((p,d)) => loop(rest, acc)
          case (p,d) :: rest                        =>
            advance(p, d) match
              case Nil => loop(rest, acc + (p -> d))
              case hs  => loop(hs ++ rest, acc + (p -> d))

      loop(List(start -> direction)) ++ Set(start -> direction)

    def energized(start: Pos, direction: Dir): Int =
      trace(start, direction).toMap.keys.size

    def maxEnergized: Int =
      val north = List.tabulate(sizeX)(x => Pos(x, 0) -> S)
      val east  = List.tabulate(sizeY)(y => Pos(sizeX - 1, y) -> W)
      val south = List.tabulate(sizeX)(x => Pos(x, sizeY - 1) -> N)
      val west  = List.tabulate(sizeY)(y => Pos(0, y) -> E)
      val entries = List(north, east, south, west).flatten
      entries.map((p,d) => energized(p,d)).max

  lazy val grid: Grid = Grid(lines.map(_.toVector))

  lazy val answer1: Long = grid.energized(start = Pos(0,0), direction = E)
  lazy val answer2: Long = grid.maxEnergized
