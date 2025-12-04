import scala.annotation.tailrec
import scala.io.*
import scala.math.Ordered.orderingToOrdered

object Day04 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Pos = (x: Int, y: Int)

  given Ordering[Pos] = Ordering.by(_.toTuple)

  val (minPos, maxPos, rolls) =
    val ls = Source.fromResource(s"input$day.txt").getLines.toVector
    val ps = for y <- ls.indices.toSet ; x <- ls.head.indices.toSet yield (x = x, y = y)
    (ps.min, ps.max, ps.filter(p => ls(p.y)(p.x) == '@'))

  extension (p: Pos)

    infix def +(that: Pos): Pos =
      (p.x + that.x, p.y + that.y)

    def neighbours: Set[Pos] =
      Set((-1,-1), (0,-1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
        .map(_ + p)
        .filter(p => p >= minPos && p <= maxPos)

  extension (rolls: Set[Pos])

    def neighbouring(p: Pos): Set[Pos] =
      p.neighbours.intersect(rolls)

    def accessible: Set[Pos] =
      rolls.filter(p => rolls.neighbouring(p).size < 4)

    @tailrec
    def clearAll: Set[Pos] =
      val remove = accessible
      if remove.isEmpty then rolls else (rolls -- remove).clearAll

  val start1 = System.currentTimeMillis
  def answer1 = rolls.accessible.size
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  def answer2 = (rolls -- rolls.clearAll).size
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
