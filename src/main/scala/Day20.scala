import scala.io.Source
import scala.collection.immutable.NumericRange

object Day20 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type IP = Long

  object IP:
    val MinValue: IP = 0x00000000
    val MaxValue: IP = 0xffffffff

  case class Range(min: IP, max: IP):

    def size: Long =
      max - min + 1

    infix def -(that: Range): Set[Range] =
      if that.min <= min && max <= that.max then
        // this   = ..#############..
        // that   = #################
        // result = .................
        Set.empty
      else if min < that.min && that.max < max then
        // this   = ..#############..
        // that   = ....########.....
        // result = ..##........##...
        Set(Range(min, that.min - 1), Range(that.max + 1, max))
      else if min <= that.max && that.max < max then
        // this   = ......########..
        // that   = ....########.....
        // result = ............##...
        Set(Range(that.max + 1, max))
      else if min < that.min && that.min <= max then
        // this   = ....###########..
        // that   = ..##########.....
        // result = ..##.............
        Set(Range(min, that.min - 1))
      else
        // this   = ....####.........
        // that   = .........###.....
        // result = ....####.........
        Set(this)

  object Ranges:

    val all: Set[Range] =
      Set(Range(IP.MinValue, IP.MaxValue))

  val blacklist: Vector[Range] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .collect:
        case s"$from-$to" => Range(from.toLong, to.toLong)
      .toVector


  def remaining(blacklist: Vector[Range]): Set[Range] =
    blacklist
      .foldLeft(Ranges.all): (result,blocked) =>
        result.flatMap: open =>
          open - blocked

  val start1  = System.currentTimeMillis
  val answer1 = remaining(blacklist).minBy(_.min).min
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = remaining(blacklist).foldLeft(0L)(_ + _.size)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
