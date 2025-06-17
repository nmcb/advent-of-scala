import scala.io.*

object Day02 extends App:

  val day: String =
    getClass.getSimpleName.filter(_.isDigit)

  enum Inst:
    case Up(delta: Long)
    case Down(delta: Long)
    case Forward(delta: Long)

  import Inst.*

  object Inst:
    def fromLine(l: String): Inst =
      l match
        case s"up $d"      => Up(d.toLong)
        case s"down $d"    => Down(d.toLong)
        case s"forward $d" => Forward(d.toLong)

  case class Sub(heading: Long = 0, depth: Long = 0, aim: Long = 0):

    def dive(n: Long): Sub = copy(depth = depth + n)
    def move(n: Long): Sub = copy(heading = heading + n)
    def tilt(n: Long): Sub = copy(aim = aim + n)

    def run(is: List[Inst], part2: Boolean = false): Sub =
      is.foldLeft(this): (cur,is) =>
        is match
          case Up(d) =>
            if part2 then cur.tilt(-d) else cur.dive(-d)
          case Down(d) =>
            if part2 then cur.tilt(d) else cur.dive(d)
          case Forward(d) =>
            if part2 then cur.move(d).dive(cur.aim * d) else cur.move(d)

    def solution: Long =
      heading * depth

  object Sub:
    val init: Sub = Sub()

  val instructions: List[Inst] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Inst.fromLine)
      .toList


  val start1  = System.currentTimeMillis
  val answer1 = Sub.init.run(instructions).solution
  println(s"Answer AOC 2021 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = Sub.init.run(instructions, part2 = true).solution
  println(s"Answer AOC 2021 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

