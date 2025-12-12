package nmcb

import Dijkstra.*
import pos.*
import pos.Pos.*
import predef.*

case class Grid[+A](matrix: Vector[Vector[A]]):
  val sizeY: Int = matrix.size
  val sizeX: Int = matrix.head.size
  val minPos: Pos = Pos.zero
  val maxPos: Pos = (sizeX - 1, sizeY - 1).toPos

  assert(matrix.forall(row => row.size == sizeX))

  def elements[A1 >: A]: Set[(Pos,A1)] =
    positions.map(p => p -> peek(p))

  lazy val positions: Set[Pos] =
    (for { x <- 0 until sizeX ; y <- 0 until sizeY } yield (x, y).toPos).toSet

  def within(p: Pos): Boolean =
    p.withinBounds(minPos, maxPos)

  def peek(p: Pos): A =
    matrix(p.y)(p.x)

  def contains[A1 >: A](p: Pos, a: A1): Boolean =
    peekOption(p).contains(a)

  def peekOption(p: Pos): Option[A] =
    Option.when(p.withinBounds(minPos, maxPos))(peek(p))

  def peekOrElse[A1 >: A](p: Pos, default: => A1): A1 =
    peekOption(p).getOrElse(default)

  def find[A1 >: A](a: A1): Option[Pos] =
    elements.find(_.element == a).map(_.pos)

  def findAll[A1 >: A](a: A1): Set[Pos] =
    elements.filter(_.element == a).map(_.pos)

  def findOne[A1 >: A](a: A1, default: => Pos = sys.error(s"not found")): Pos =
    find(a).getOrElse(default)

  def filter[A1 >: A](f: ((Pos,A1)) => Boolean): Set[(Pos,A1)] =
    elements.filter(f)

  def filterNot[A1 >: A](f: ((Pos,A1)) => Boolean): Set[(Pos,A1)] =
    elements.filterNot(f)

  def map[B](f: A => B): Grid[B] =
    Grid(matrix.map(_.map(f)))

  def row(y: Int): Vector[A] =
    matrix(y)

  def updated[A1 >: A](p: Pos, a: A1): Grid[A1] =
    Grid(matrix.updated(p.y, row(p.y).updated(p.x, a)))

  def asString: String =
    matrix.map(_.mkString("")).mkString("\n")

  def extractPath[A1 >: A](from: A1, to: A1, node: A1): (Pos,Pos,Grid[A1]) =
    val fromPos  = findOne(from)
    val toPos    = findOne(to)
    val cleared  = updated(fromPos, node).updated(toPos, node)
    (fromPos, toPos, cleared)

  def dropRow(y: Int): Grid[A] =
    Grid(matrix.zipWithIndex.filter((r,i) => i != y).map((r,i) => r))

  def transpose: Grid[A] =
    Grid(matrix.transpose)

  def flipX: Grid[A] =
    Grid(matrix.map(_.reverse))

  def flipY: Grid[A] =
    Grid(matrix.reverse)

  def rotateCW: Grid[A] =
    def rotateMatrixCW(matrix: Vector[Vector[A]]): Vector[Vector[A]] =
      if matrix.isEmpty then
        matrix
      else
        Vector.tabulate(sizeX, sizeY)((x,y) => matrix(sizeX - 1 - y)(x))
    Grid(rotateMatrixCW(matrix))

object Grid:

  def fromLines(lines: Iterator[String]): Grid[Char] =
    Grid(lines.map(_.toVector).toVector)

  def fromLines(lines: Seq[String]): Grid[Char] =
    Grid(lines.map(_.toVector).toVector)

  def fromString[A](s: String): Grid[Char] =
    fromLines(s.trim.split('\n').iterator)

  def fromMatrix[A](matrix: Iterator[Seq[A]]): Grid[A] =
    Grid(matrix.iterator.to(Vector).map(_.iterator.to(Vector)))

  def fill[A](sizeX: Int, sizeY: Int, default: A): Grid[A] =
    Grid(Vector.fill(sizeX, sizeY)(default))

  extension [A](g: (Pos,Pos,Grid[A]))
    def from: Pos = g._1
    def to: Pos   = g._2
    def cleared: Grid[A] = g._3

    def shortest: Vector[Pos] =
      Dijkstra
        .run(Graph.fromGrid(cleared, cleared.peek(from)), from)
        .pathTo(to)
        .toTrail