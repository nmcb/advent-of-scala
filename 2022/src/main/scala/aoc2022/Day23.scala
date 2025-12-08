package aoc2022

import nmcb.*
import scala.annotation.tailrec

object Day23 extends AoC:

  case class Pos(x: Int, y: Int):
    def +(p: Pos): Pos = copy(x = x + p.x, y = y + p.y)

  object Pos:
    def fromString(y: Int, s: String): Set[Pos] =
      s.zipWithIndex.foldLeft(Set.empty):
        case (a,('#',x)) => a + Pos(x,y)
        case (a,_)       => a

  val positions: Set[Pos] =
    lines
      .zipWithIndex
      .foldLeft(Set.empty):
        case (a,(s,y)) => a ++ Pos.fromString(y,s)

  enum Dir:
    case N extends Dir
    case S extends Dir
    case W extends Dir
    case E extends Dir

  import Dir.*

  case class Mat(elves: Set[Pos], dirs: Seq[Dir] = Seq(N, S, W, E)):

    override def toString: String =
      val minX = elves.map(_.x).min
      val maxX = elves.map(_.x).max
      val minY = elves.map(_.y).min
      val maxY = elves.map(_.y).max
      val chars =
        for
          y <- minY to maxY
          x <- minX to maxX
          c = if elves.contains(Pos(x,y)) then '#' else '.'
        yield
          c

      chars
        .grouped(maxX - minX + 1)
        .map(_.mkString("","","\n"))
        .mkString("\n","","\n")

    def neighbours(f: Pos): Int =
      val offset = Set(
        Pos(-1,-1),
        Pos( 0,-1),
        Pos( 1,-1),
        Pos(-1, 0),
        Pos( 1, 0),
        Pos(-1, 1),
        Pos( 0, 1),
        Pos( 1, 1),
      )
      offset.count(d => elves.contains(f + d))

    def valid(f: Pos, d: Dir): Boolean =
      val offset = Set(-1, 0, 1)
      d match
        case N => offset.forall(d => !elves.contains(f + Pos( d,-1)))
        case S => offset.forall(d => !elves.contains(f + Pos( d, 1)))
        case W => offset.forall(d => !elves.contains(f + Pos(-1, d)))
        case E => offset.forall(d => !elves.contains(f + Pos( 1, d)))

    def propose(f: Pos, ds: Seq[Dir]): Option[Pos] =
      def proposal(d: Dir): Pos =
        d match
          case N => f + Pos( 0,-1)
          case S => f + Pos( 0, 1)
          case W => f + Pos(-1, 0)
          case E => f + Pos( 1, 0)
      ds.find(d => valid(f,d)).map(proposal)

    val proposals: Map[Pos,Option[Pos]] =
      elves.map(e => if neighbours(e) == 0 then e -> None else e -> propose(e, dirs)).toMap

    val moves: Set[Pos] =
      val counts = proposals.view.values.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
      proposals.map((e,maybe) => maybe.map(move => if counts(move) == 1 then move else e).getOrElse(e)).toSet

    def next: Mat =
      Mat(moves, dirs.tail :+ dirs.head)

    def countEmpty: Int =
      val minX = elves.map(_.x).min
      val maxX = elves.map(_.x).max
      val minY = elves.map(_.y).min
      val maxY = elves.map(_.y).max
      val size =
        for
        x <- minX to maxX
        y <- minY to maxY
        if !elves.contains(Pos(x,y))
      yield 1
      size.sum

  @tailrec
  def solve2(m: Mat, c: Int = 1): Int =
    val next = m.next
    if next.elves == m.elves then
      c
    else
      solve2(next, c + 1)


  lazy val answer1: Int = (1 to 10).foldLeft(Mat(positions))((m, _) => m.next).countEmpty
  lazy val answer2: Long = solve2(Mat(positions))
