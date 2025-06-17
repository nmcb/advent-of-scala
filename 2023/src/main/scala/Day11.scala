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

  case class Pos(x: Int, y: Int)

  case class Image(galaxies: Vector[Vector[Char]]):

    lazy val positions: Vector[Pos] =
      galaxies.zipWithIndex.flatMap((l,y) =>
        l.zipWithIndex.flatMap((c,x) =>
          Option.when(c == '#')(Pos(x, y))))

    def empty(gs: Vector[Vector[Char]]): Set[Int] =
      gs.zipWithIndex.flatMap((line,i) => if line.forall(_ == '.') then Some(i) else None).toSet

    lazy val emptyY: Set[Int] =
      empty(galaxies)

    lazy val emptyX: Set[Int] =
      empty(galaxies.transpose)

    def expand(a: Int, b: Int, indices: Set[Int], multiplier: Long = 1L): Long =
      val range = if a <= b then a until b else b until a
      range.map(n => if indices.contains(n) then multiplier else 1L).sum

    def distances(multiplier: Long): Vector[Long] =
      positions.zipWithIndex.flatMap((g0,n) =>
        positions.drop(n).map(g1 =>
          expand(g0.x, g1.x, emptyX, multiplier) + expand(g0.y, g1.y, emptyY, multiplier)))

    override def toString: String =
      galaxies.map(_.mkString("")).mkString("", "\n", "\n")


  val start1: Long  = System.currentTimeMillis
  val answer1: Long = image.distances(2L).sum
  println(s"Answer AOC 2023 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = image.distances(1000000L).sum
  println(s"Answer AOC 2023 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
