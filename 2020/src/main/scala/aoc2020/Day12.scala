package aoc2020

import nmcb.*
import scala.annotation.*

object Day12 extends AoC:

  case class Action(override val toString: String):
    def code: Char = toString.head
    def by: Int    = toString.tail.toInt

  object Action:
    def fromLine(str: String): Action = Action(str)

  case class Ship(x: Int = 0, y: Int = 0, face: Char = 'E'):

    @tailrec
    private final def turn(current: Char, direction: Char, by: Int): Char =
      if (by == 0) current else (current,direction) match
        case ('N','R') => turn('E', direction, by - 90)
        case ('E','R') => turn('S', direction, by - 90)
        case ('S','R') => turn('W', direction, by - 90)
        case ('W','R') => turn('N', direction, by - 90)
        case ('N','L') => turn('W', direction, by - 90)
        case ('E','L') => turn('N', direction, by - 90)
        case ('S','L') => turn('E', direction, by - 90)
        case ('W','L') => turn('S', direction, by - 90)
        case _         => sys.error(s"two faced=${(current,direction)}")


    infix def perform(a: Action): Ship =
      a.code match
        case 'N'       => copy(y = y + a.by)
        case 'S'       => copy(y = y - a.by)
        case 'E'       => copy(x = x + a.by)
        case 'W'       => copy(x = x - a.by)
        case 'L' | 'R' => copy(face = turn(face, a.code, a.by))
        case 'F'       => face match
                            case 'N' => copy(y = y + a.by)
                            case 'S' => copy(y = y - a.by)
                            case 'E' => copy(x = x + a.by)
                            case 'W' => copy(x = x - a.by)
                            case _   => sys.error(s"flat on its face=$face")
        case _   => sys.error(s"code contains a error='${a.code}'")

    def manhattan: Int =
      x.abs + y.abs

  case class WayPoint(x: Int, y: Int):

    @tailrec private final def turn(current: Char, by: Int, x0: Int = x, y0: Int = y): WayPoint =
      if by == 0 then copy(x = x0, y = y0) else current match
        case 'R' => turn(current, by - 90, y0, 0 - x0)
        case 'L' => turn(current, by - 90, 0 - y0, x0)
        case _   => sys.error(s"code contains a coding error='$current'")

    def perform(a: Action): WayPoint =
        a.code match
          case 'N'     => copy(y = y + a.by)
          case 'S'     => copy(y = y - a.by)
          case 'E'     => copy(x = x + a.by)
          case 'W'     => copy(x = x - a.by)
          case degrees => turn(degrees, a.by)

  case class WayPointedShip(x: Int = 0, y: Int = 0, wp: WayPoint = WayPoint(10, 1)):

    infix def perform(a: Action): WayPointedShip =
      a.code match
        case 'N' | 'S' | 'E' | 'W' | 'L' | 'R' => copy(wp = wp.perform(a))
        case 'F'                               => copy(x = x + wp.x * a.by, y = y + wp.y * a.by)
        case _   => sys.error(s"code contains a coding error='${a.code}'")

    def manhattan: Int =
      x.abs + y.abs

  lazy val actions: Vector[Action] = lines.map(Action.fromLine)

  lazy val answer1: Int = actions.foldLeft(Ship())(_ perform _).manhattan
  lazy val answer2: Int = actions.foldLeft(WayPointedShip())(_ perform _).manhattan
