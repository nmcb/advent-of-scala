import scala.annotation.*
import scala.io.*

object Day11 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Stone = String

  extension (s: Stone) def dropLeadingZeros: Stone =
    val dropped = s.dropWhile(_ == '0')
    if dropped.isEmpty then "0" else dropped

  type StoneCount  = (stone: Stone, count: Long)


  def update(ss: StoneCount): Vector[StoneCount] =
    val handle = ss.stone match
      case "0" =>
        Vector("1")
      case s if s.length % 2 == 0 =>
        val (l, r) = s.splitAt(s.length / 2)
        Vector(l.dropLeadingZeros, r.dropLeadingZeros)
      case n =>
        Vector((n.toLong * 2024).toString)

    handle.map(_ -> ss.count) :+ (ss.stone -> -ss.count)

  val stones: Vector[StoneCount] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(' ')
      .toVector
      .map(_ -> 1L)

  def blink(n: Int, stones: Vector[StoneCount]): Long =
    @tailrec
    def loop(result: Vector[StoneCount], blinked: Int = 0): Long =
      if blinked >= n then
        result.map(_.count).sum
      else
        loop(
          result
            .foldLeft(Vector.empty[StoneCount]): (updates, ss: StoneCount) =>
              updates ++ update(ss)
            .groupMapReduce(_.stone)(_.count)(_ + _)
            .foldLeft(result.map(_.toTuple).toMap): (next, ss: StoneCount) =>
                next.updatedWith(ss.stone):
                  case Some(count) if count == (-ss.count) => None
                  case Some(count)                         => Some(count + ss.count)
                  case None                                => Some(ss.count)
            .toVector,
          blinked + 1
        )
    loop(stones)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = blink(25, stones)
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = blink(75, stones)
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
