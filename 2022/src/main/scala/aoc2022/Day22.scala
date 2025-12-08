package aoc2022

import nmcb.*
import scala.io.*

object Day22 extends AoC:

  val puzzle: Seq[String] =
    Source
      .fromResource(s"$day.txt")
      .getLines
      .toIndexedSeq

  val map: Seq[String] =
    puzzle.takeWhile(l => !l.isBlank)

  val path: String =
    puzzle.drop(map.size + 1).head

  val tiles: Array[Array[Char]] =
    Array.fill(map.length + 2, map.head.length + 2)(' ')

  map.zipWithIndex.foreach((r,y) => r.zipWithIndex.foreach((t,x) => tiles(y + 1)(x + 1) = t))

  enum Dir:
    case N extends Dir
    case E extends Dir
    case S extends Dir
    case W extends Dir

  import Dir.*

  case class Pos(x: Int, y: Int, dir: Dir, path: String):

    def right: Pos =
      dir match
        case N => copy(dir = E)
        case E => copy(dir = S)
        case S => copy(dir = W)
        case W => copy(dir = N)

    def left: Pos =
      dir match
        case N => copy(dir = W)
        case E => copy(dir = N)
        case S => copy(dir = E)
        case W => copy(dir = S)

    private def has(y: Int, x: Int, t: Char): Boolean = tiles(y)(x) == t
    def tile(y: Int, x: Int): Boolean = has(y, x, '.')
    def wall(y: Int, x: Int): Boolean = has(y, x, '#')
    def mapped(c: Char): Boolean = c == '.' || c == '#'

    def step1(dy: Int, dx: Int, steps: Int)(wrap: => Pos): Pos =
      if      tile(y + dy, x + dx) then copy(x = x + dx, y = y + dy).move1(steps - 1)
      else if wall(y + dy, x + dx) then this
      else wrap

    def move1(steps: Int): Pos =
      if (steps == 0)
        this
      else
        dir match
          case N =>
            step1(-1,0, steps) {
              val ny = tiles.transpose.apply(x).lastIndexWhere(mapped)
              if wall(ny, x) then this else copy(y = ny).move1(steps - 1)
            }
          case S =>
            step1(1,0, steps) {
              val ny = tiles.transpose.apply(x).indexWhere(mapped)
              if wall(ny, x) then this else copy(y = ny).move1(steps - 1)
            }
          case E =>
            step1(0,1, steps) {
              val nx = tiles(y).indexWhere(mapped)
              if wall(y, nx) then this else copy(x = nx).move1(steps - 1)
              }
          case W =>
            step1(0, -1, steps) {
              val nx = tiles(y).lastIndexWhere(mapped)
              if wall(y, nx) then this else copy(x = nx).move1(steps - 1)
            }

    def step2(dy: Int, dx: Int, steps: Int)(wrap: => Pos): Pos =
      if      tile(y + dy, x + dx) then copy(x = x + dx, y = y + dy).move2(steps - 1)
      else if wall(y + dy, x + dx) then this
      else wrap

    def move2(steps: Int): Pos =
      if steps == 0 then
        this
      else
        dir match
          case N =>
            step2(-1, 0, steps) {
              if x <= 50 then
                val nx = 51
                val ny = x + 50
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = E).move2(steps - 1)
              else if x <= 100 then
                val nx = 1
                val ny = x + 100
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = E).move2(steps - 1)
              else
                val nx = x - 100
                val ny = 200
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = N).move2(steps - 1)
            }
          case S =>
            step2(1, 0, steps) {
              if x <= 50 then
                val nx = x + 100
                val ny = 1
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = S).move2(steps - 1)
              else if x <= 100 then
                val nx = 50
                val ny = x + 100
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = W).move2(steps - 1)
              else
                val nx = 100
                val ny = x - 50
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = W).move2(steps - 1)
            }
          case E =>
            step2(0, 1, steps) {
              if y <= 50 then
                val nx = 100
                val ny = (y - 151).abs
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = W).move2(steps - 1)
              else if y <= 100 then
                val nx = y + 50
                val ny = 50
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = N).move2(steps - 1)
              else if y <= 150 then
                val nx = 150
                val ny = (y - 151).abs
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = W).move2(steps - 1)
              else
                val nx = y - 100
                val ny = 150
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = N).move2(steps - 1)
            }
          case W =>
            step2(0, -1, steps) {
              if y <= 50 then
                val nx = 1
                val ny = (y - 151).abs
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = E).move2(steps - 1)
              else if y <= 100 then
                val nx = y - 50
                val ny = 101
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = S).move2(steps - 1)
              else if y <= 150 then
                val nx = 51
                val ny = (y - 151).abs
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = E).move2(steps - 1)
              else
                val nx = y - 100
                val ny = 1
                if wall(ny, nx) then this else copy(y = ny, x = nx, dir = S).move2(steps - 1)
            }
    def hasNext: Boolean =
      path.nonEmpty

    private def next(f: Int => Pos): Pos =
      path match
        case s if s.startsWith("R") => right.copy(path = s.drop(1))
        case s if s.startsWith("L") => left.copy(path = s.drop(1))
        case s => f(s.takeWhile(_.isDigit).toInt).copy(path = s.dropWhile(_.isDigit))

    def next1: Pos = next(move1)
    def next2: Pos = next(move2)

    def value: Long =
      val adder =
        dir match
          case E => 0
          case S => 1
          case W => 2
          case N => 3
      y * 1000L + 4L * x + adder

  object Pos:
    def start: Pos = Pos(tiles(1).indexOf('.'), 1, E, path)

  
  lazy val answer1: Long =
    var pos = Pos.start
    while (pos.hasNext) pos = pos.next1
    pos.value

  lazy val answer2: Long =
    var pos = Pos.start
    while (pos.hasNext) pos = pos.next2
    pos.value
