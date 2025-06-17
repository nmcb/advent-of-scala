import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App:

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  val banks: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .mkString
      .split("\\s")
      .map(_.trim.toInt)
      .toVector

  case class Area(banks: Vector[Int]):

    extension (index: Int) private def next: Int =
      if index + 1 >= banks.size then 0 else index + 1

    @tailrec
    private def distribute(amount: Int, index: Int, accumulator: Vector[Int]): Vector[Int] =
      if amount <= 0 then
        accumulator
      else
        val update = accumulator(index) + 1
        distribute(
          accumulator = accumulator.updated(index, update),
          index       = index.next,
          amount      = amount - 1
        )

    def redistribute: Vector[Area] =
      @tailrec
      def loop(current: Area, seen: Vector[Area] = Vector.empty): Vector[Area] =
        if seen.contains(current) then
          seen :+ current
        else
          val blocks = current.banks.max
          val index  = current.banks.indexOf(blocks)
          val bank   = current.banks.updated(index, 0)
          val next   = Area(distribute(blocks, index.next, bank))
          loop(next, seen :+ current)
      loop(this)

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    Area(banks).redistribute.size - 1

  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    val seen = Area(banks).redistribute
    val last = seen.last
    seen.dropWhile(_ != last).size - 1

  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
