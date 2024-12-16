package nmcb

import Grid.*
import Pos.*

case class Grid[A](matrix: Vector[Vector[A]]):

  val sizeY: Int = matrix.size
  val sizeX: Int = matrix.head.size
  val minPos: Pos = zero
  val maxPos: Pos = (sizeX - 1, sizeY - 1).toPos

  lazy val elements: Seq[(Pos,A)] =
    positions.map(p => p -> peek(p))

  lazy val positions: Seq[Pos] =
    for { x <- 0 until sizeX ; y <- 0 until sizeY } yield (x, y).toPos

  def within(p: Pos): Boolean =
    p.withinBounds(min = Pos.zero, max = Pos(sizeX, sizeY))

  def peek(p: Pos): A =
    matrix(p.y)(p.x)

  def peekOption(p: Pos): Option[A] =
    Option.when(p.withinBounds(minPos, maxPos))(matrix(p.y)(p.x))

  def peekOrElse(p: Pos, default: => A): A =
    peekOption(p).getOrElse(default)

  def find(a: A): Pos =
    elements.find(_.element == a).get.pos

object Grid:

  def fromLines(lines: Iterator[String]): Grid[Char] =
    Grid(lines.map(_.toVector).toVector)

  extension [A](tuple: (Pos,A))
    def pos: Pos   = tuple._1
    def element: A = tuple._2