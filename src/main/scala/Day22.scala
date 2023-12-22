import scala.annotation.tailrec
import scala.io.Source

object Day22 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Stack(stack: Vector[Box]):

    lazy val settled: Vector[Box] =
      stack
        .sortBy(_.min.z)
        .foldLeft(Vector.empty[Box]): (dropped, box) =>
          val height =
            (1 to Int.MaxValue)
              .find: h =>
                val dropping = box.drop(h)
                dropping.min.z == 0 || dropped.exists(_ intersects dropping)
              .getOrElse(sys.error("no dropping height found")) - 1
          dropped :+ box.drop(height)

    val supportedBy: Map[Box, Vector[Box]] =
      settled
        .map: box =>
          box -> settled
            .filter(_ intersects box.drop(1))
            .filterNot(_ == box)
        .toMap

    def disintegrable: Int =
      val supports: Vector[Box] =
        settled
          .map: box =>
            supportedBy(box)
              .filter(_ intersects box.drop(1))
          .filter(_.size == 1)
          .map(_.head)
      stack.toSet.size - supports.toSet.size

    def disintegrated: Int =
      val supports: Map[Box, Vector[Box]] =
        supportedBy
          .toVector
          .flatMap: (box, support) =>
            support.map(_ -> box)
          .groupMap(_._1)(_._2)

      def disintegrates(box: Box): Int =
        def loop(todo: Set[Box], found: Set[Box] = Set.empty): Int =
          if todo.isEmpty then
            found.size - 1
          else
            val box     = todo.head
            val next    = todo - box
            val support = supports.getOrElse(box, Set.empty[Box]).toSet
            val visited = found + box
            val add     = support.filter(x => (supportedBy(x).toSet -- visited).isEmpty)
            loop(next ++ add, visited)
        loop(Set(box))

      settled.map(disintegrates).sum

  val stack: Stack =
    Stack(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(Box.fromString)
        .toVector)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = stack.disintegrable
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = stack.disintegrated
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

  // Geometry

  case class Pos(x: Int, y: Int, z: Int):
    def -(that: Pos): Pos = Pos(x - that.x, y - that.y, z - that.z)
    def +(that: Pos): Pos = Pos(x + that.x, y + that.y, z + that.z)
    def min(that: Pos): Pos = Pos(x min that.x, y min that.y, z min that.z)
    def max(that: Pos): Pos = Pos(x max that.x, y max that.y, z max that.z)
    def >=(that: Pos): Boolean = x >= that.x && y >= that.y && z >= that.z
    def <=(that: Pos): Boolean = x <= that.x && y <= that.y && z <= that.z

  case class Box(min: Pos, max: Pos):

    def intersect(that: Box): Option[Box] =
      val maxmin = min max that.min
      val minmax = max min that.max
      if maxmin <= minmax then
        Some(Box(maxmin, minmax))
      else
        None

    def intersects(that: Box): Boolean =
      intersect(that).isDefined

  object Box:

    def fromString(s: String): Box =
      s match
        case s"$x1,$y1,$z1~$x2,$y2,$z2" =>
          val p1 = Pos(x1.toInt, y1.toInt, z1.toInt)
          val p2 = Pos(x2.toInt, y2.toInt, z2.toInt)
          Box(p1, p2)

  extension (box: Box) def drop(height: Int): Box =
    val offset = Pos(0, 0, height)
    Box(box.min - offset, box.max - offset)
