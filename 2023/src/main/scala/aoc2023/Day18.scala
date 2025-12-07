package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day18 extends AoC:

  case class Op(direction: Char, length: Int)

  object Op:
    def fromStringPart(s: String): Op =
      s match
        case s"$direction $length (#$hex)" =>
          Op(direction.head, length.toInt)

    def fromHexPart(s: String): Op =
      s match
        case s"$direction $length (#$hex)" =>
          val direction =
            hex.last match
              case '0' => 'R'
              case '1' => 'D'
              case '2' => 'L'
              case '3' => 'U'
          Op(direction, Integer.parseInt(hex.init, 16))

  case class Pos(x: Int, y: Int):
    def -(b: Pos): Pos = Pos(x - b.x, y - b.y)
    def +(b: Pos): Pos = Pos(x + b.x, y + b.y)
    infix def min(b: Pos): Pos = Pos(math.min(x, b.x), math.min(y, b.y))
    infix def max(b: Pos): Pos = Pos(math.max(x, b.x), math.max(y, b.y))
    def >=(b: Pos): Boolean = x >= b.x && y >= b.y
    def <=(b: Pos): Boolean = x <= b.x && y <= b.y
    def ×(that: Pos): Long = x.toLong * that.y.toLong - that.x.toLong * y.toLong

    infix def move1(op: Op): Vector[Pos] =
      op match
        case Op('U', length) => Vector.tabulate(length)(dy => Pos(x, y - dy - 1))
        case Op('D', length) => Vector.tabulate(length)(dy => Pos(x, y + dy + 1))
        case Op('L', length) => Vector.tabulate(length)(dx => Pos(x - dx - 1, y))
        case Op('R', length) => Vector.tabulate(length)(dx => Pos(x + dx + 1, y))
        case Op(dir, length) => sys.error(s"invalid dir=$dir, length=$length")

    infix def move2(op: Op): Pos =
      op match
        case Op('U', length) => Pos(x, y - length)
        case Op('D', length) => Pos(x, y + length)
        case Op('L', length) => Pos(x - length, y)
        case Op('R', length) => Pos(x + length, y)
        case Op(dir, length) => sys.error(s"invalid dir=$dir, length=$length")


    def neighbours: Set[Pos] =
      val xs = Set(Pos(-1, 0), Pos(1, 0))
      val ys = Set(Pos(0, -1), Pos(0, 1))
      (xs ++ ys).map(this + _)

  object Pos:
    def zero: Pos = Pos(0, 0)

    def grid(min: Pos, max: Pos): IndexedSeq[Pos] =
      for
        x <- min.x to max.x
        y <- min.y to max.y
      yield
        Pos(x, y)

  /** cubic meter = one point - data driven flood algorithm with some set arithmetic */
  def dig1(operations: Vector[Op]): Long =
    val holes: Set[Pos] = operations.foldLeft(Vector(Pos.zero))((a,o) => a ++ a.last.move1(o)).toSet
    val min = holes.reduce(_ min _) - Pos(1, 1)
    val max = holes.reduce(_ max _) + Pos(1, 1)

    @tailrec
    def flood(todo: List[Pos], visited: Set[Pos] = Set.empty): Set[Pos] =
      todo match
        case Nil =>
          visited
        case cur :: rest =>
          val reached =
            cur
              .neighbours
              .diff(holes)
              .diff(visited)
              .filter(box => box >= min && box <= max)

          flood(rest ++ reached, visited ++ reached + cur)

    val outer = flood(List(min))
    val dug   = Pos.grid(min, max).toSet diff outer
    dug.size.toLong

  lazy val operationsPart1: Vector[Op] = lines.map(Op.fromStringPart)

  /** one operation = one polygon - geometric driven shoelace formula made discrete with pick's theorem */
  def dig2(operations: Vector[Op]): Long =

    // shoelace formula
    def shoeLace(vertices: Vector[Pos]): Long =
      val result =
        (vertices :+ vertices.head)
          .sliding(2)
          .map(m => m(0) × m(1))
          .sum / 2
      result.abs

    val vertices = operations.scanLeft(Pos.zero)(_ move2 _)
    val geometricArea = shoeLace(vertices)

    // pick's theorem
    val edge = operations.map(_.length.toLong).sum
    val interior = geometricArea - edge / 2 + 1
    val discreteArea = edge + interior

    discreteArea

  assert(answer1 == dig2(operationsPart1)) // 1000x faster

  lazy val operationsPart2: Vector[Op] = lines.map(Op.fromHexPart)


  lazy val answer1: Long = dig1(operationsPart1)
  lazy val answer2: Long = dig2(operationsPart2)
