package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day10 extends AoC:

  enum Dir:
    case N
    case S
    case E
    case W

    def innerR: Dir =
      this match
        case N => E
        case E => S
        case S => W
        case W => N

    def opposite: Dir =
      this match
        case N => S
        case E => W
        case S => N
        case W => E

  import Dir.*

  enum Tile(val directions: Set[Dir]):
    case Vertical   extends Tile(Set(N,S))
    case Horizontal extends Tile(Set(E,W))
    case BendNE     extends Tile(Set(N,E))
    case BendNW     extends Tile(Set(N,W))
    case BendSW     extends Tile(Set(S,W))
    case BendSE     extends Tile(Set(S,E))
    case Ground     extends Tile(Set())
    case Start      extends Tile(Set()) // handle as special case
    def opposite(d: Dir): Option[Dir] =
      if directions.contains(d.opposite) then directions.find(_ != d.opposite) else None

  import Tile.*

  object Tile:
    def fromChar(c: Char): Tile =
      c match
        case '|' => Vertical
        case '-' => Horizontal
        case 'L' => BendNE
        case 'J' => BendNW
        case '7' => BendSW
        case 'F' => BendSE
        case '.' => Ground
        case 'S' => Start

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Option[Pos] =
      d match
        case N if y > 0    => Some(Pos(x, y - 1))
        case E if x < maxX => Some(Pos(x + 1, y))
        case S if y < maxY => Some(Pos(x, y + 1))
        case W if x > 0    => Some(Pos(x - 1, y))
        case _             => None

  object Pos:
    given Ordering[Pos] = Ordering.by(p => (p.x, p.y))

  lazy val tiles: Map[Pos,Tile] =
    lines
      .zipWithIndex
      .foldLeft(Map.empty[Pos,Tile]){ case (a,(l,y)) =>
        l.zipWithIndex.foldLeft(a){ case (a,(c,x)) =>
          a + (Pos(x,y) -> Tile.fromChar(c))
        }
      }

  lazy val maxX: Int = tiles.keys.map(_.x).max
  lazy val maxY: Int = tiles.keys.map(_.y).max

  def pathTo(from: Pos, dir: Dir): Option[Pos] =
    tiles(from) match
      case Ground => None
      case tile   => tile.opposite(dir).flatMap(from.move)


  def pathsFrom(from: Pos, dir: Dir): Vector[(Pos,Dir)] =
    @tailrec
    def loop(p: Pos, d: Dir, a: Vector[(Pos,Dir)] = Vector.empty): Vector[(Pos,Dir)] =
      p.move(d) match
        case None    => a
        case Some(n) =>
          tiles(n).opposite(d) match
            case None    => a
            case Some(o) => loop(n, o, a :+ (n,o))
    loop(from, dir, Vector((from, dir)))


  lazy val start: Pos =
    tiles.find((_,t) => t == Start).map((p,_) => p).getOrElse(sys.error("no start tile"))

  /** trial and error on direction - seems we need to go west */
  lazy val path: Vector[(Pos,Dir)] =
    pathsFrom(start, W)

  def area(path: Vector[(Pos,Dir)]): Set[Pos] =

    val cacheX: Vector[Pos] = path.sortBy((p,_) => p.x).map((p,_) => p)
    val cacheY: Vector[Pos] = path.sortBy((p,_) => p.y).map((p,_) => p)

    /** right handed enclosure */
    def scan(pos: Pos, dir: Dir): Set[Pos] =
      dir match
        case N if pos.y > 0 =>
          val r = cacheY.filter(p => p.x == pos.x && p.y < pos.y)
          if r.nonEmpty then (r.last.y + 1 until pos.y).map(y => Pos(pos.x, y)).toSet else Set.empty
        case E if pos.x < maxX =>
          val r = cacheX.filter(p => p.y == pos.y && p.x > pos.x)
          if r.nonEmpty then (pos.x + 1 until r.head.x).map(x => Pos(x, pos.y)).toSet else Set.empty
        case S if pos.y < maxY =>
          val r = cacheY.filter(p => p.x == pos.x && p.y > pos.y)
          if r.nonEmpty then (pos.y + 1 until r.head.y).map(y => Pos(pos.x, y)).toSet else Set.empty
        case W if pos.x > 0 =>
          val r = cacheX.filter(p => p.y == pos.y && p.x < pos.x)
          if r.nonEmpty then (r.last.x + 1 until pos.x).map(x => Pos(x, pos.y)).toSet else Set.empty
        case _ => Set.empty

    def loop(todo: Vector[(Pos,Dir)], prev: Dir, acc: Set[Pos]): Set[Pos] =
      todo match
        case (p, n) +: rest =>
          tiles(p) match
            case Vertical   if prev.opposite == N => loop(rest, n, acc ++ scan(p, W))
            case Vertical   if prev.opposite == S => loop(rest, n, acc ++ scan(p, E))
            case Horizontal if prev.opposite == E => loop(rest, n, acc ++ scan(p, N))
            case Horizontal if prev.opposite == W => loop(rest, n, acc ++ scan(p, S))
            case BendNE     if prev.opposite == N => loop(rest, n, acc ++ scan(p, S))
            case BendNW     if prev.opposite == W => loop(rest, n, acc ++ scan(p, S))
            case BendSE     if prev.opposite == E => loop(rest, n, acc ++ scan(p, N))
            case BendSW     if prev.opposite == S => loop(rest, n, acc ++ scan(p, N))
            case _                                => loop(rest, n, acc)
        case _ => acc

    val (p, d) = path.head
    loop(path.tail, d, scan(p, d.innerR))


  lazy val answer1: Int = path.length / 2
  lazy val answer2: Int = area(path).size
