import scala.io.*
import scala.math.Integral.Implicits.*

object Day13 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Long, y: Long)

  case class Machine(a: Pos, b: Pos, p: Pos):

    def cost(m: Long, n: Long): Long =
      m * 3 + n

    def solve(offset: Long): Option[Long] =
      val px       = p.x + offset
      val py       = p.y + offset
      val div      = a.x * b.y - b.x * a.y
      val (ma, ra) = (px * b.y - py * b.x) /% div
      val (nb, rb) = (py * a.x - px * a.y) /% div
      if ra == 0 & rb == 0 then Some(cost(ma, nb)) else None

  val machines: Vector[Machine] =
    val lines = Source.fromResource(s"input$day.txt").getLines.toVector
    val as = lines.collect:
      case s"Button A: X+$x, Y+$y" => Pos(x.toLong, y.toLong)
    val bs = lines.collect:
      case s"Button B: X+$x, Y+$y" => Pos(x.toLong, y.toLong)
    val ps = lines.collect:
      case s"Prize: X=$x, Y=$y"    => Pos(x.toLong, y.toLong)
    as.zip(bs).zip(ps).map:
      case ((a,b),p) => Machine(a, b, p)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = machines.flatMap(_.solve(offset = 0L)).sum
  println(s"Answer AOC 2024 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = machines.flatMap(_.solve(offset = 10000000000000L)).sum
  println(s"Answer AOC 2024 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
