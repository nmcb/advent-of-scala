import scala.io.*

object Day05 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Range = (min: Long, max: Long)

  val (ranges: Vector[Range], ingredients: Vector[Long]) =
    val Array(top, bottom) = Source.fromResource(s"input$day.txt").mkString.split("\n\n")

    val rs = top.linesIterator
      .map:
        case s"$min-$max" => (min = min.toLong, max = max.toLong)
      .toVector

    val is = bottom.linesIterator.map(_.toLong).toVector

    (rs, is)


  extension (range: Range)

    inline def contains(l: Long): Boolean =
      l >= range.min && l <= range.max

    inline def size: Long =
      range.max - range.min + 1


  val start1  = System.currentTimeMillis
  def answer1 = ingredients.count(i => ranges.exists(_.contains(i)))
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  extension (ranges: Vector[Range])

    def merge: Vector[Range] =
      ranges.sortBy(_.min).foldLeft(Vector.empty[Range]):
        case (tail :+ last, test) if test.min <= last.max + 1 => tail :+ (last.min, last.max max test.max)
        case (result, range)                                  => result :+ range

  val start2  = System.currentTimeMillis
  def answer2 = ranges.merge.map(_.size).sum

  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
