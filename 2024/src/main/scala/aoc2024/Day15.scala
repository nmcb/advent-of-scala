package aoc2024

import nmcb.*

import scala.annotation.*

object Day15 extends AoC:

  type Dir = Char

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Pos =
      d match
        case '^' => copy(y = y - 1)
        case '>' => copy(x = x + 1)
        case 'v' => copy(y = y + 1)
        case '<' => copy(x = x - 1)

    def coordinate: Int =
      100 * y + x

  type Move = (Pos,Pos)
  
  extension (m: Move) def from = m._1
  extension (m: Move) def to   = m._2

  case class Grid(grid: Map[Pos,Char]):
    lazy val sizeX: Int = grid.keys.maxBy(_.x).x + 1
    lazy val sizeY: Int = grid.keys.maxBy(_.y).y + 1
    lazy val robotPosition: Pos     = grid.find((_, c) => c == '@').map(_._1).head
    lazy val boxPositions: Set[Pos] = grid.filter((_, c) => c == 'O' | c == '[').keySet

    override def toString: String =
      val representation = for {
        y <- 0 until sizeY
        x <- 0 until sizeX
        p = Pos(x, y)
      } yield grid(p)
      representation.mkString("").grouped(sizeX).mkString("\n")

    def isFree(p: Pos): Boolean = grid(p) == '.'
    def isWall(p: Pos): Boolean = grid(p) == '#'
    def isBox(p: Pos): Boolean  = grid(p) == 'O' | grid(p) == ']' | grid(p) == '['

    def moves(p: Pos, d: Dir): Set[Move] =
      @tailrec
      def loop(todo: Set[Pos], updates: Set[Move] = Set.empty): Set[Move] =

        def widen(c: Set[Pos]): Set[Pos] =
          val pl = c.minBy(_.x)
          val pr = c.maxBy(_.x)
          val l = if (d == '^' | d == 'v') & grid(pl) == ']' then Set(pl.copy(x = pl.x - 1)) else Set.empty
          val r = if (d == '^' | d == 'v') & grid(pr) == '[' then Set(pr.copy(x = pr.x + 1)) else Set.empty
          l ++ c ++ r

        def unblocked(ms: Set[Pos]): Boolean = ms.forall(isFree)
        def blocked(ms: Set[Pos]): Boolean   = ms.exists(isWall)

        val moves = widen(todo).map(from => from -> from.move(d))
        val tos   = moves.map(_.to)

        if      unblocked(tos) then updates ++ moves
        else if blocked(tos)   then Set.empty
        else                        loop(todo = widen(tos).filter(isBox), updates = updates ++ moves)

      loop(Set(p))

    infix def push(d: Dir): Grid =
      val updates = moves(robotPosition, d)
      if updates.isEmpty then
        this
      else
        val updated: Map[Pos,Char] =
          updates.foldLeft(grid):
            case (g,(f,t)) => g.updated(t, grid(f))

        val updatedAndCleaned: Map[Pos,Char] =
          val froms = updates.map(_.from)
          val tos   = updates.map(_.to)
          froms.diff(tos).foldLeft(updated)((g, p) => g.updated(p, '.'))

        Grid(updatedAndCleaned)

    def resize: Grid =
      Grid(
        grid.flatMap:
          case (p,'#') => Map(p.copy(x = p.x * 2) -> '#', p.copy(x = (p.x * 2) + 1) -> '#')
          case (p,'.') => Map(p.copy(x = p.x * 2) -> '.', p.copy(x = (p.x * 2) + 1) -> '.')
          case (p,'O') => Map(p.copy(x = p.x * 2) -> '[', p.copy(x = (p.x * 2) + 1) -> ']')
          case (p,'@') => Map(p.copy(x = p.x * 2) -> '@', p.copy(x = (p.x * 2) + 1) -> '.')
          case (p, c ) => sys.error(s"invalid element: p=$p, c=$c")
      )


  val (grid: Grid, moves: List[Dir]) =
    val Array(top, bottom) = input.split("\n\n").map(_.trim)
    val positions = for {
      (l, y) <- top.split("\n").zipWithIndex
      (c, x) <- l.trim.zipWithIndex
    } yield Pos(x, y) -> c
    val moves = bottom.filterNot(_ == '\n').toList
    (Grid(positions.toMap), moves)

  lazy val answer1: Int = moves.foldLeft(grid)(_ push _).boxPositions.map(_.coordinate).sum
  lazy val answer2: Int = moves.foldLeft(grid.resize)(_ push _).boxPositions.map(_.coordinate).sum
