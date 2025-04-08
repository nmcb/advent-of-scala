import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day18 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Value = String | Long

  enum Instruction:
    case SND(x: Value)
    case SET(x: String, y: Value)
    case ADD(x: String, y: Value)
    case MUL(x: String, y: Value)
    case MOD(x: String, y: Value)
    case RCV(x: String)
    case JGZ(x: Value, y: Value)

  import Instruction.*

  val assembly: Vector[Instruction] =
    def parse(s: String): Value = Try(s.toLong).getOrElse(s)
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"snd $x"    => SND(parse(x))
        case s"set $x $y" => SET(x, parse(y))
        case s"add $x $y" => ADD(x, parse(y))
        case s"mul $x $y" => MUL(x, parse(y))
        case s"mod $x $y" => MOD(x, parse(y))
        case s"rcv $x"    => RCV(x)
        case s"jgz $x $y" => JGZ(parse(x), parse(y))
      .toVector

  println(assembly)

  case class Synthesizer(assembly: Vector[Instruction], pc: Long, registers: Map[String,Long], sound: List[Long], recovered: List[Long]):

    def value(v: Value): Long =
      v match
        case l: Long   => l
        case r: String => registers(r)

    def process(instruction: Instruction): Synthesizer =
      println(s"instruction=$instruction, synth=$this")
      instruction match
        case SND(x)    => copy(pc = pc + 1, sound = value(x) :: sound)
        case SET(x, y) => copy(pc = pc + 1, registers = registers + (x -> value(y)))
        case ADD(x, y) => copy(pc = pc + 1, registers = registers + (x -> (value(x) + value(y))))
        case MUL(x, y) => copy(pc = pc + 1, registers = registers + (x -> (value(x) * value(y))))
        case MOD(x, y) => copy(pc = pc + 1, registers = registers + (x -> (value(x) % value(y))))
        case RCV(x)    => copy(pc = pc + 1, registers = registers + (x -> sound.head), recovered = sound.head :: recovered)
        case JGZ(x, y) => if value(x) > 0 then copy(pc = pc + value(y)) else copy(pc = pc + 1)

    @tailrec
    final def run: Synthesizer =
      if pc < 0 || pc >= assembly.length || recovered.exists(_ != 0) then this else process(assembly(pc.toInt)).run

  object Synthesizer:
    def load(assembly: Vector[Instruction]): Synthesizer =
      Synthesizer(assembly = assembly, pc = 0, Map.empty[String,Long].withDefaultValue(0), List.empty[Long], List.empty)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = Synthesizer.load(assembly).run.recovered.last
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
