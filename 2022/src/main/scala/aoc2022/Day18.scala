package aoc2022

import nmcb.*

import scala.annotation.*
import scala.math.*

object Day18 extends AoC:

  val boxes: Set[Box] =
    lines
      .map:
        case s"$x,$y,$z" => Box(x.toInt, y.toInt, z.toInt)
      .toSet

  case class Box(x: Int, y: Int, z: Int):
    infix def -(b: Box): Box = Box(x - b.x, y - b.y, z - b.z)
    infix def +(b: Box): Box = Box(x + b.x, y + b.y, z + b.z)
    infix def min(b: Box): Box = Box(math.min(x, b.x), math.min(y, b.y), math.min(z, b.z))
    infix def max(b: Box): Box = Box(math.max(x, b.x), math.max(y, b.y), math.max(z, b.z))
    infix def >=(b: Box): Boolean = x >= b.x && y >= b.y && z >= b.z
    infix def <=(b: Box): Boolean = x <= b.x && y <= b.y && z <= b.z

    def neighbours: Set[Box] =
      val xs = Set(Box(-1, 0, 0), Box(1, 0, 0))
      val ys = Set(Box( 0,-1, 0), Box(0, 1, 0))
      val zs = Set(Box( 0, 0,-1), Box(0, 0, 1))
      (xs ++ ys ++ zs).map(this + _)



  def solve2(): Int =
    val min = boxes.reduce(_ min _) - Box(1,1,1)
    val max = boxes.reduce(_ max _) + Box(1,1,1)

    @tailrec
    def flood(todo: List[Box], visited: Set[Box]): Set[Box] =
      todo match
        case Nil =>
          visited
        case cur :: rest =>
          val reached =
            cur
              .neighbours
              .diff(boxes)
              .diff(visited + cur)
              .filter(n => n >= min && n <= max)

          flood(rest ++ reached, visited ++ reached + cur)

    val outer = flood(List(min), Set.empty)
    boxes.toSeq.map(_.neighbours.count(outer.contains)).sum

  
  lazy val answer1: Int = boxes.toList.map(p => 6 - (p.neighbours intersect boxes).size).sum
  lazy val answer2: Int = solve2()
