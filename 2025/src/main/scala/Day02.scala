import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Range(min: Long, max: Long)

  val input: Vector[Range] =
      Source
        .fromResource(s"input$day.txt")
        .mkString
        .trim
        .split(',')
        .map:
          case s"$min-$max" => Range(min.toLong, max.toLong)
        .toVector

  def doubles(id: Long): Boolean =
    val s = id.toString
    s.length % 2 == 0 && (s.drop(s.length / 2) == s.dropRight(s.length / 2))

  def solve(input: Vector[Range], invalid: Long => Boolean): Long =
    val purged =
      for
        range <- input
        id    <- range.min to range.max
        if invalid(id)
      yield
        id
    purged.sum

  val start1: Long       = System.currentTimeMillis
  lazy val answer1: Long = solve(input, doubles)
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def repeats(id: Long): Boolean =
    val s = id.toString
    val doubled = s + s
    doubled.substring(1, doubled.length - 1).contains(s)

  val start2: Long       = System.currentTimeMillis
  lazy val answer2: Long = solve(input, repeats)
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
