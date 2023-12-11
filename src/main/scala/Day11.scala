import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day11 extends App:

  val day: String = this.getClass.getName.drop(3).init

  lazy val image: Image =
    Image(
      Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.toVector)
      .toVector)

  case class Pos(x: Int, y: Int):

    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

    def *(k: Int): Pos =
      Pos(k * x, k * y)

    def manhattan(that: Pos): Int =
      (x - that.x).abs + (y - that.y).abs

    def <=(that: Pos): Boolean =
      x <= that.x && y <= that.y

    def min(that: Pos): Pos =
      Pos(x min that.x, y min that.y)

    def max(that: Pos): Pos =
      Pos(x max that.x, y max that.y)

    def cross(that: Pos): Int =
      x * that.y - that.x * y



  case class Image(galaxies: Vector[Vector[Char]]):
    def expanded: Image =

      def expand(a: Vector[Vector[Char]], l: Vector[Char]): Vector[Vector[Char]] =
        if l.count(_ == '.') == l.length then a :+ l :+ l else a :+ l

      val lines   = galaxies.foldLeft(Vector.empty)(expand)
      val columns = lines.transpose.foldLeft(Vector.empty)(expand)
      Image(columns.transpose)

    lazy val positions: Vector[Pos] =
      galaxies.zipWithIndex.flatMap((l,y) => l.zipWithIndex.flatMap((c,x) => Option.when(c == '#')(Pos(x,y))))

    lazy val distances1: Vector[Int] =
      positions.zipWithIndex.flatMap((g0,n) => positions.drop(n).map(g1 => g0 manhattan g1))

    def empty(gs: Vector[Vector[Char]]): Set[Int] =
      gs.zipWithIndex.flatMap((l,i) => if l.forall(_ == '.') then Some(i) else None).toSet

    lazy val emptyY: Set[Int] =
      empty(galaxies)

    lazy val emptyX: Set[Int] =
      empty(galaxies.transpose)

    def expand(a: Int, b: Int, indices: Set[Int]): Long =
      val range = if (a <= b) a until b else b until a
      range.map(n => if indices.contains(n) then 1000000L else 1L).sum

    lazy val distances2: Vector[Long] =
      positions
        .zipWithIndex
        .flatMap((g0,n) => positions.drop(n).map(g1 => expand(g0.x, g1.x, emptyX) + expand(g0.y, g1.y, emptyY)))

    override def toString: String =
      galaxies.map(_.mkString("")).mkString("", "\n", "\n")


  val start1: Long = System.currentTimeMillis
  val answer1: Int = image.expanded.distances1.sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = image.distances2.sum
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
