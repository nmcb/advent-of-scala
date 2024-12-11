import scala.io.*
import scala.annotation.*

object Day11 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Stone = String

  extension (s: Stone) def dropLeadingZeros: Stone =
    val dropped = s.dropWhile(_ == '0')
    if dropped.isEmpty then "0" else dropped

  def handle(stone: Stone): Vector[Stone] =
    stone match
      case "0" =>
        Vector("1")
      case s if s.length % 2 == 0 =>
        val (l, r) = s.splitAt(s.length / 2)
        Vector(l.dropLeadingZeros, r.dropLeadingZeros)
      case n =>
        Vector((n.toLong * 2024).toString)

  val stones: Vector[(Stone,Long)] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .trim
      .split(' ')
      .toVector
      .map(_ -> 1L)

  def blink(n: Int, stones: Vector[(Stone,Long)]): Long =
    @tailrec
    def loop(result: Vector[(Stone,Long)], blinked: Int = 0): Long =
      if blinked >= n then
        result.map(_._2).sum
      else
        loop(
          result
            .foldLeft(Vector.empty[(Stone, Long)]):
              case (updates, (stone, count)) => updates ++ handle(stone).map(_ -> count) :+ (stone -> (-count))
            .groupMapReduce(_._1)(_._2)(_ + _)
            .foldLeft(result.toMap):
              case (next, (update, delta)) =>
                next.updatedWith(update):
                  case None                             => Some(delta)
                  case Some(count) if count == (-delta) => None
                  case Some(count)                      => Some(count + delta)
            .toVector,
          blinked + 1
        )
    loop(stones)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = blink(25, stones)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = blink(75, stones)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
