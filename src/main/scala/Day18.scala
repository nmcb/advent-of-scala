import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.*

object Day18 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Op(direction: Char, length: Int, color: String)

  object Op:
    def fromString(s: String): Op =
      s match
        case s"$direction $length ($color)" => Op(direction.head, length.toInt, color)

  case class Box(x: Int, y: Int, z: Int):
    def -(b: Box): Box = Box(x - b.x, y - b.y, z - b.z)

    def +(b: Box): Box = Box(x + b.x, y + b.y, z + b.z)

    def min(b: Box): Box = Box(math.min(x, b.x), math.min(y, b.y), math.min(z, b.z))

    def max(b: Box): Box = Box(math.max(x, b.x), math.max(y, b.y), math.max(z, b.z))

    def >=(b: Box): Boolean = x >= b.x && y >= b.y && z >= b.z

    def <=(b: Box): Boolean = x <= b.x && y <= b.y && z <= b.z

    def move(op: Op): Vector[Box] =
      op match
        case Op('U', length, color) => Vector.tabulate(length)(dy => Box(x, y - dy - 1, 0))
        case Op('D', length, color) => Vector.tabulate(length)(dy => Box(x, y + dy + 1, 0))
        case Op('L', length, color) => Vector.tabulate(length)(dx => Box(x - dx - 1, y, 0))
        case Op('R', length, color) => Vector.tabulate(length)(dx => Box(x + dx + 1, y, 0))

    def neighbours: Set[Box] =
      val xs = Set(Box(-1, 0, 0), Box(1, 0, 0))
      val ys = Set(Box(0, -1, 0), Box(0, 1, 0))
      val zs = Set(Box(0, 0, -1), Box(0, 0, 1))
      (xs ++ ys ++ zs).map(this + _)

  object Box:
    def zero: Box = Box(0, 0, 0)

    def boxes(min: Box, max: Box): Set[Box] =
      val result =
        for
          x <- min.x to max.x
          y <- min.y to max.y
          z <- min.z to max.z
        yield
          Box(x, y, z)
      result.toSet

  case class Ground(holes: Set[Box]):

    def mkString(z: Int): String =
      val layer = holes.filter(_.z == z)
      val rangeX = layer.map(_.x).min to layer.map(_.x).max
      val rangeY = layer.map(_.y).min to layer.map(_.y).max
      val chars =
        for
          y <- rangeY
          x <- rangeX
        yield
          if holes.contains(Box(x, y, z)) then '#' else '.'
      chars.grouped(rangeX.size).map(_.mkString("")).mkString("\n")


    def dug: Ground =
      val min = holes.reduce(_ min _) - Box(1, 1, 1)
      val max = holes.reduce(_ max _) + Box(1, 1, 1)

      def flood(todo: List[Box], visited: Set[Box] = Set.empty): Set[Box] =
        todo match
          case Nil =>
            visited
          case cur :: rest =>
            val reached =
              cur
                .neighbours
                .filter(_.z == 0)
                .diff(holes)
                .diff(visited)
                .filter(box => box >= min && box <= max)

            flood(rest ++ reached, visited ++ reached + cur)

      val outer = flood(List(min))
      println(s"outer=${outer.size}")
      val dug   = Box.boxes(min, max).filter(_.z == 0) diff outer
      println(s"dug=${dug.size}")
      Ground(dug)


  object Ground:
    def empty: Ground =
      Ground(Set.empty)

    def from(ops: Vector[Op]): Ground =
      val (_, holes) =
        ops.foldLeft((Box.zero, Vector(Box.zero))):
          case ((cur,acc), op) =>
            val dug = cur.move(op)
            (dug.last, acc ++ dug)
      Ground(holes.toSet)

  lazy val operations: Vector[Op] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Op.fromString)
      .toVector

  println(Ground.from(operations).dug.mkString(0))

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Ground.from(operations).dug.holes.size
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


//  val start2: Long = System.currentTimeMillis
//  val answer2: Int = ???
//  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

