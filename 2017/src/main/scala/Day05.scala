import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App:

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  val jumps: Array[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim.toInt)
      .toArray

  case class CPU(mem: Array[Int], update: Int => Int, pc: Int = 0, steps: Int = 0):
    @tailrec
    final def run: CPU =
      mem.lift(pc) match
        case None =>
          this
        case Some(offset) =>
          CPU(mem.updated(pc, update(offset)), update, pc + offset, steps + 1).run

  val start1: Long =
    System.currentTimeMillis

  val answer1: Int =
    CPU(jumps, offset => offset + 1).run.steps

  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Int =
    CPU(jumps, offset => if offset >= 3 then offset - 1 else offset + 1).run.steps

  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
