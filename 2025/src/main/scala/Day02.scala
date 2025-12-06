import scala.collection.immutable.NumericRange
import scala.io.*

object Day02 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Range = NumericRange[Long]

  val input: Vector[Range] =
      Source
        .fromResource(s"input$day.txt")
        .mkString
        .trim
        .split(',')
        .map:
          case s"$min-$max" => min.toLong to max.toLong
        .toVector

  def doubles(id: Long): Boolean =
    val s = id.toString
    s.length % 2 == 0 && (s.drop(s.length / 2) == s.dropRight(s.length / 2))

  def filter(input: Vector[Range], invalid: Long => Boolean): Vector[Long] =
    for
      range <- input
      id    <- range
      if invalid(id)
    yield
      id

  val start1: Long       = System.currentTimeMillis
  def answer1: Long = filter(input, doubles).sum
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def repeats(id: Long): Boolean =
    val s = id.toString
    val doubled = s + s
    doubled.substring(1, doubled.length - 1).contains(s)

  val start2: Long       = System.currentTimeMillis
  def answer2: Long = filter(input, repeats).sum
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
