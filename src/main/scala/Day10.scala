import scala.annotation.*
import scala.io.*
import scala.math.Integral.Implicits.*
import scala.math.Ordering.Implicits.*

object Day10 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int):
    infix def +(p: Pos): Pos = Pos(x + p.x, y + p.y)
    infix def -(p: Pos): Pos = Pos(x - p.x, y - p.y)
    infix def *(i: Int): Pos = Pos(x * i, y * i)
    infix def min(that: Pos): Pos = Pos(x min that.x, y min that.y)
    infix def max(that: Pos): Pos = Pos(x max that.x, y max that.y)


  case class Box(min: Pos, max: Pos):
    
    infix def union(that: Box): Box =
      Box(min min that.min, max max that.max)

    def area: Long =
      val delta = max - min
      delta.x.toLong * delta.y.toLong

  object Box:
    def apply(p: Pos): Box = Box(p, p)

    def bounding(ps: IterableOnce[Pos]): Box =
      ps.iterator.map(apply).reduce(_ union _)


  case class Point(position: Pos, velocity: Pos):
    def step(t: Int): Point = Point(position + velocity * t, velocity)

  object Point:
    def fromString(s: String): Point =
      s.trim match
        case s"position=<$x, $y> velocity=<$vx, $vy>" =>
          Point(Pos(x.trim.toInt, y.trim.toInt), Pos(vx.trim.toInt, vy.trim.toInt))


  // returns the range containing the first domain which yields at least `x` as its codomain, starting from `min`.
  def exponentialLower[A, B](f: A => B, min: A)(x: B)(using int: Integral[A], ord: Ordering[B]): (A, A) =
    val a0 = int.zero
    val a1 = int.one
    val a2 = int.fromInt(2)

    @tailrec
    def loop(lo: A, hi: A): (A, A) = if f(min + hi) >= x then (min + lo + a1, min + hi + a1) else loop(hi, a2 * hi)

    if f(min) < x then loop(a0, a1) else  (min, min + a1)

  // returns the first domain, which yields at least `x` as its codomain.
  def binaryLower[A, B](f: A => B, min: A, max: A)(x: B)(using int: Integral[A], ord: Ordering[B]): A =
    val a1 = int.one
    val a2 = int.fromInt(2)

    @tailrec
    def loop(lo: A, hi: A): A =
      if lo >= hi then
        lo
      else
        val mid = (hi - lo) / a2 + lo
        if f(mid) >= x then loop(lo, mid) else loop(mid + a1, hi)

    loop(min, max)

  def exponentialBinaryLower[A, B](f: A => B, min: A)(x: B)(using Integral[A], Ordering[B]): A =
    val (min2, max) = exponentialLower(f, min)(x)
    binaryLower(f, min2, max)(x)


  def boundingArea(points: Vector[Point]): Int => Long =
    t => Box.bounding(points.map(_.step(t).position)).area

  val points: Vector[Point] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Point.fromString)
      .toVector

  extension (points: Vector[Point]) def asString: String =
    val positions     = points.map(_.position)
    val Box(min, max) = Box.bounding(points.map(_.position))
    val positionsSet  = positions.toSet

    (min.y to max.y).foldLeft(StringBuffer())((sb,y) => (min.x to max.x).foldLeft(sb)((sb,x) =>
      if positionsSet.contains(Pos(x,y)) then sb.append('#') else sb.append('.')
    ).append('\n')).toString

  val (sky, fastest) =

    def slope(t: Int): Long =
      boundingArea(points)(t + 1) - boundingArea(points)(t)

    val fastest: Int =
      exponentialBinaryLower(slope,0)(0L)

    (points.map(_.step(fastest)), fastest)

  val start1  = System.currentTimeMillis
  val answer1 = sky.asString
  println(s"Day $day answer part 1:\n$answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = fastest
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
