import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.*

object Day18 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

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

    def min(b: Pos): Pos = Pos(math.min(x, b.x), math.min(y, b.y))

    def max(b: Pos): Pos = Pos(math.max(x, b.x), math.max(y, b.y))

    def >=(b: Pos): Boolean = x >= b.x && y >= b.y

    def <=(b: Pos): Boolean = x <= b.x && y <= b.y

    def cross(that: Pos): Long =
      x.toLong * that.y.toLong - that.x.toLong * y.toLong

    def move1(op: Op): Vector[Pos] =
      op match
        case Op('U', length) => Vector.tabulate(length)(dy => Pos(x, y - dy - 1))
        case Op('D', length) => Vector.tabulate(length)(dy => Pos(x, y + dy + 1))
        case Op('L', length) => Vector.tabulate(length)(dx => Pos(x - dx - 1, y))
        case Op('R', length) => Vector.tabulate(length)(dx => Pos(x + dx + 1, y))

    def move2(op: Op): Pos =
      op match
        case Op('U', length) => Pos(x, y - length)
        case Op('D', length) => Pos(x, y + length)
        case Op('L', length) => Pos(x - length, y)
        case Op('R', length) => Pos(x + length, y)


    def neighbours: Set[Pos] =
      val xs = Set(Pos(-1, 0), Pos(1, 0))
      val ys = Set(Pos(0, -1), Pos(0, 1))
      (xs ++ ys).map(this + _)

  object Pos:
    def zero: Pos = Pos(0, 0)

    def grid(min: Pos, max: Pos): Set[Pos] =
      val result =
        for
          x <- min.x to max.x
          y <- min.y to max.y
        yield
          Pos(x, y)
      result.toSet

  /** cubic meter = one unit - data driven flood algorithm with some set arithmetic */
  def dig1(operations: Vector[Op]) =
    val holes: Set[Pos] = operations.foldLeft(Vector(Pos.zero))((a,o) => a ++ a.last.move1(o)).toSet
    val min = holes.reduce(_ min _) - Pos(1, 1)
    val max = holes.reduce(_ max _) + Pos(1, 1)

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
    val dug   = Pos.grid(min, max) diff outer
    dug.size.toLong

  lazy val operationsPart1: Vector[Op] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Op.fromStringPart)
      .toVector

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = dig1(operationsPart1)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** one operation = one polygon - geometric driven shoelace formula made discrete with pick's theorem */
  def dig2(operations: Vector[Op]): Long =
    def shoeLace(poss: Vector[Pos]): Long =
      val result =
        (poss.last +: poss)
          .sliding(2)
          .map:
            case Seq(a, b) => (a, b)
            case _ => sys.error(s"boom!")
          .map(_ cross _)
          .sum / 2
      result.abs

    val vertices = operations.scanLeft(Pos.zero)(_ move2 _)
    val area = shoeLace(vertices)
    val edge = operations.map(_.length.toLong).sum
    val interior = area - edge / 2 + 1 // pick's theorem
    edge + interior

  assert(answer1 == dig2(operationsPart1)) // 1000x faster

  lazy val operationsPart2: Vector[Op] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Op.fromHexPart)
      .toVector

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = dig2(operationsPart2)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
