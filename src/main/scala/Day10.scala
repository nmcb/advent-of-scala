import scala.io.Source

object Day10 extends App:

  val day: String = this.getClass.getName.drop(3).init

  sealed trait Inst
  case object Nop                             extends Inst
  case class  Add(value: Int, steps: Int = 2) extends Inst

  lazy val instructions: List[Inst] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(_.trim)
      .map {
        case s"noop"    => Nop
        case s"addx $v" => Add(v.toInt)
      }
      .toList

  case class CPU(is: List[Inst], cycle: Int = 0, x: Int = 1):

    val sync = List(20, 60, 100, 140, 180, 220).map(_ - 1)

    def nextCycle: CPU =
      is match
        case Nop      :: r => CPU(r            , cycle + 1, x    )
        case Add(v,2) :: r => CPU(Add(v,1) :: r, cycle + 1, x    )
        case Add(v,1) :: r => CPU(r            , cycle + 1, x + v)
        case _ => sys.error("boom!")

    val signalStrength: Int =
      x * (cycle + 1)

    val sprite: List[Int] =
      List(x - 1, x , x + 1)

    val draw: Char =
      if sprite.contains(cycle % 40) then '#' else '.'

  def solve1(cpu: CPU, acc: Int = 0): Int =
    if cpu.cycle > cpu.sync.max then
      acc
    else if cpu.sync.contains(cpu.cycle) then
      solve1(cpu.nextCycle, acc + cpu.signalStrength)
    else
      solve1(cpu.nextCycle, acc)


  val start1: Long = System.currentTimeMillis
  val answer1: Int = solve1(CPU(instructions))
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def solve2(cpu: CPU, pixels: String = ""): String =
    if cpu.is.isEmpty then
      pixels.grouped(40).mkString("\n")
    else
      solve2(cpu.nextCycle, pixels :+ cpu.draw)

  val start2: Long = System.currentTimeMillis
  val answer2: String = solve2(CPU(instructions))
  println(s"Answer day $day part 2: \n$answer2 [${System.currentTimeMillis - start1}ms]")
