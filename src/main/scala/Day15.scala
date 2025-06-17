import scala.annotation.tailrec
import scala.io.*

object Day15 extends App:

  val day: String = this.getClass.getName.drop(3).init

  lazy val operations: Vector[Op] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .filterNot(_ == '\n')
      .mkString
      .split(',')
      .map(Op.apply)
      .toVector


  def hash(s: String): Int =
    s.foldLeft(0)((h, c) => (h + c.toInt) * 17 % 256)

  case class Op(line: String):

    lazy val label: String =
      line.takeWhile(c => c != '-' && c != '=')

    lazy val remove: Option[String] =
      Option.when(line.contains('-'))(label)

    lazy val update: Option[(String,Int)] =
      Option.when(line.contains('='))(label, line.split('=')(1).toInt)

  case class Boxes(boxes: Map[Int,Vector[(String,Int)]]):

    def remove(label: String): Boxes =
      Boxes(boxes.updatedWith(hash(label)):
        case Some(lenses) => Some(lenses.filterNot((l,f) => l == label))
        case None         => None
      )

    def update(label: String, focal: Int): Boxes =
      Boxes(boxes.updatedWith(hash(label)):
        case Some(lenses) if lenses.exists((l,_) => l == label) =>
          Some(lenses.foldLeft(Vector.empty):
            case (a,(l,f)) => if l == label then a :+ (l, focal) else a :+ (l, f))
        case Some(lenses) =>
          Some(lenses :+ (label, focal))
        case None =>
          Some(Vector((label, focal)))
      )

    infix def process(op: Op): Boxes =
      op.remove.map(remove).orElse(op.update.map(update)).getOrElse(sys.error("noop"))

    def power: Int =
      boxes.foldLeft(0):
        case (p, (box, lenses)) =>
          lenses.zipWithIndex.foldLeft(p):
            case (pp, ((label, focal), index)) =>
              pp + (box + 1) * (index + 1) * focal


  object Boxes:
    def empty: Boxes =
      Boxes(Map.empty)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = operations.map(op => hash(op.line)).sum
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = operations.foldLeft(Boxes.empty)(_ process _).power
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
