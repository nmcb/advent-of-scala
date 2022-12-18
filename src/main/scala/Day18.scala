import scala.annotation.*
import scala.io.*
import scala.math.*

object Day18 extends App:

  val day: String = this.getClass.getName.drop(3).init

  val input: Set[Box] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim)
      .map { case s"$x,$y,$z" => Box(x.toInt, y.toInt, z.toInt) }
      .toSet

  case class Box(x: Int, y: Int, z: Int):
    import Box.*

    def -(b: Box): Box = Box(x - b.x, y - b.y, z - b.z)
    def +(b: Box): Box = Box(x + b.x, y + b.y, z + b.z)
    def min(b: Box): Box = Box(math.min(x, b.x), math.min(y, b.y), math.min(z, b.z))
    def max(b: Box): Box = Box(math.max(x, b.x), math.max(y, b.y), math.max(z, b.z))
    def >=(b: Box): Boolean = x >= b.x && y >= b.y && z >= b.z
    def <=(b: Box): Boolean = x <= b.x && y <= b.y && z <= b.z

    def neighbours: Set[Box] =
      val xs = Set(Box(-1, 0, 0), Box(1, 0, 0))
      val ys = Set(Box( 0,-1, 0), Box(0, 1, 0))
      val zs = Set(Box( 0, 0,-1), Box(0, 0, 1))
      (xs ++ ys ++ zs).map(this + _)

  val start1: Long = System.currentTimeMillis
  val answer1 = input.toList.map(p => 6 - (p.neighbours intersect input).size).sum
  println(s"Answer day $day part 2: $answer1 [${System.currentTimeMillis - start1}ms]")


  def solve2: Int =
    val min = input.reduce(_ min _) - Box(1,1,1)
    val max = input.reduce(_ max _) + Box(1,1,1)

    import scala.collection.immutable.Queue

    def flood(todo: List[Box], visited: Set[Box]): Set[Box] =
      todo match
        case Nil =>
          visited
        case cur :: rest =>
          val reached =
            cur
              .neighbours
              .diff(input)
              .diff(visited + cur)
              .filter(n => n >= min && n <= max)

          flood(rest.concat(reached), visited ++ reached + cur)

    val outer = flood(List(min), Set.empty)
    input.toSeq.map(_.neighbours.count(outer.contains)).sum

  val start2: Long = System.currentTimeMillis
  val answer2 = solve2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
