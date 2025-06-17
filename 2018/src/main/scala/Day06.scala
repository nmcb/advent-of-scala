import scala.*
import scala.io.*
import scala.math.*

object Day06 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    infix def manhattan(p: Pos): Int = abs(x - p.x) + abs(y - p.y)

  case class Grid(coordinates: Vector[Pos]):

    val minX: Int = coordinates.minBy(_.x).x
    val maxX: Int = coordinates.maxBy(_.x).x

    val minY: Int = coordinates.minBy(_.y).y
    val maxY: Int = coordinates.maxBy(_.y).y

    val positions: Vector[Pos] =
      (for x <- minX to maxX ; y <- minY to maxY yield Pos(x,y)).toVector

    type UnitDistance = (Pos,Int)

    extension (unitDistance: UnitDistance)
      def coordinate: Pos = unitDistance._1
      def distance: Int   = unitDistance._2

    val closest: Vector[(Pos,Pos)] =
      positions.flatMap: p =>
        coordinates.map(c => (c, c manhattan p))
          .sortBy(_.distance).take(2) match
            case a +: b +: _ if a.distance == b.distance => None
            case      a +: _                             => Some(p,a.coordinate)
            case           _                             => sys.error(s"no distance found for position: $p")

    val areas: Map[Pos,Int] =
      closest.groupMapReduce((_,c) => c)((_,_) => 1)(_+_)

    def infinite(coordinate: Pos): Boolean =
      closest.exists((p,c) => c == coordinate && (p.x == minX || p.x == maxX || p.y == minY || p.y == maxY))

    def largestAreaSize: Int =
      val (_, size) = areas.filterNot((p,_) => infinite(p)).maxBy((_,s) => s)
      size

    def manhattanSum(position: Pos): Int =
      coordinates.map(position.manhattan).sum

    def withinManhattanSumLimit(limit: Int): Vector[Pos] =
      positions.filter(p => manhattanSum(p) < limit)


  val coordinates: Vector[Pos] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map { case s"$x, $y" => Pos(x.toInt, y.toInt) }
        .toVector

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Grid(coordinates).largestAreaSize
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Grid(coordinates).withinManhattanSumLimit(10000).size
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
