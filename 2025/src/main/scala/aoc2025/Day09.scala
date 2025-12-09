package aoc2025

import nmcb.*
import predef.*

object Day09 extends AoC:

  type Pos = (x: Long, y: Long)

  extension (pp: (Pos,Pos))

    def area: Long =
      val dx = (pp.left.x - pp.right.x).abs
      val dy = (pp.left.y - pp.right.y).abs
      (dx  + 1) * (dy + 1)

  val corners: Vector[Pos] = lines.collect:
    case s"$x,$y" => (x = x.toLong, y = y.toLong)

  def line(p: Pos, q: Pos): Set[Pos] =
    for
      x <- ((p.x min q.x) to (p.x max q.x)).toSet
      y <- ((p.y min q.y) to (p.y max q.y)).toSet
    yield
      (x, y)

  def perimeter(corners: Vector[Pos]): Set[Pos] =
    corners.zip(corners.tail :+ corners.head).flatMap(line).toSet

  def isValid(p: Pos, q: Pos, perimeter: Set[Pos]): Boolean =
    val xMin = p.x min q.x
    val xMax = p.x max q.x
    val yMin = p.y min q.y
    val yMax = p.y max q.y
    !perimeter.exists(r => xMin < r.x && r.x < xMax && yMin < r.y && r.y < yMax)

  val perimeter: Set[Pos] = perimeter(corners)
  
  given order: Ordering[(Pos,Pos)] = Ordering.by(_.area) 

  lazy val answer1: Long = corners.pairs(using order).drain.area
  lazy val answer2: Long = corners.pairs(using order.reverse).findFirst((a,b) => isValid(a, b, perimeter)).area

