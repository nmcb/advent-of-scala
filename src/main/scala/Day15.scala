import scala.io.Source

object Day15 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Disc(nr: Int, positions: Int, position: Int):
    def passWhenDroppedAt(time: Int): Boolean =
      (nr + position + time) % positions == 0

  type Discs = Vector[Disc]

  lazy val discs: Discs =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"Disc #$nr has $positions positions; at time=0, it is at position $position." =>
          Disc(nr.toInt, positions.toInt, position.toInt)
      .toVector

  def validDrop(discs: Discs)(time: Int): Boolean =
    discs.forall(_.passWhenDroppedAt(time))

  val start1  = System.currentTimeMillis
  val answer1 = Iterator.from(0).filter(validDrop(discs)).next
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = Iterator.from(0).filter(validDrop(discs :+ Disc(nr = 7, positions = 11, position = 0))).next
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
