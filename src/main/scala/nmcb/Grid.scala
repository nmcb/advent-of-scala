package nmcb

import predef.*

import Pos.*
import Dijkstra.*

case class Grid[A](matrix: Vector[Vector[A]]):

  val sizeY: Int = matrix.size
  val sizeX: Int = matrix.head.size
  val minPos: Pos = Pos.zero
  val maxPos: Pos = (sizeX - 1, sizeY - 1).toPos

  lazy val elements: Set[(Pos,A)] =
    positions.map(p => p -> peek(p))

  lazy val positions: Set[Pos] =
    (for { x <- 0 until sizeX ; y <- 0 until sizeY } yield (x, y).toPos).toSet

  def within(p: Pos): Boolean =
    p.withinBounds(minPos, maxPos)

  def peek(p: Pos): A =
    matrix(p.y)(p.x)

  def contains(p: Pos, a: A): Boolean =
    peekOption(p).contains(a)

  def peekOption(p: Pos): Option[A] =
    Option.when(p.withinBounds(minPos, maxPos))(peek(p))

  def peekOrElse(p: Pos, default: => A): A =
    peekOption(p).getOrElse(default)

  def find(a: A): Option[Pos] =
    elements.find(_.element == a).map(_.pos)

  def findAll(a: A): Set[Pos] =
    elements.filter(_.element == a).map(_.pos)

  def findOne(a: A, default: => Pos = sys.error(s"not found")): Pos =
    find(a).getOrElse(default)

  def filter(f: ((Pos,A)) => Boolean): Set[(Pos,A)] =
    elements.filter(f)

  def filterNot(f: ((Pos,A)) => Boolean): Set[(Pos,A)] =
    elements.filterNot(f)

  def map[B](f: A => B): Grid[B] =
    Grid(matrix.map(_.map(f)))

  def row(y: Int): Vector[A] =
    matrix(y)

  def updated(p: Pos, a: A): Grid[A] =
    Grid(matrix.updated(p.y, row(p.y).updated(p.x, a)))

  def asString: String =
    matrix.map(_.mkString("")).mkString("\n")

  def extractPath(from: A, to: A, node: A): (Pos,Pos,Grid[A]) =
    val fromPos  = findOne(from)
    val toPos    = findOne(to)
    val cleared  = updated(fromPos, node).updated(toPos, node)
    (fromPos, toPos, cleared)


object Grid:

  def fromLines(lines: Iterator[String]): Grid[Char] =
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