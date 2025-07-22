import scala.annotation.tailrec
import scala.io.Source

object Day23 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Register  = String
  type Registers = Map[Register,Int]

  extension (registers: Registers)

    def valueOf(r: Register): Int =
      registers(r)

    def update(r: Register, f: Int => Int): Registers =
      registers + (r -> f(valueOf(r)))

    def even(r: Register): Boolean =
      valueOf(r) % 2 == 0

    def isOne(r: Register): Boolean =
      valueOf(r) == 1


  enum Inst:
    case HLF(r: Register)
    case TPL(r: Register)
    case INC(r: Register)
    case JMP(offset: Int)
    case JIE(r: Register, offset: Int)
    case JIO(r: Register, offset: Int)

  import Inst.*

  val instructions: Vector[Inst] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"hlf $r"     => HLF(r)
        case s"tpl $r"     => TPL(r)
        case s"inc $r"     => INC(r)
        case s"jmp $o"     => JMP(o.toInt)
        case s"jie $r, $o" => JIE(r, o.toInt)
        case s"jio $r, $o" => JIO(r, o.toInt)
      .toVector

  case class CPU(prog: Vector[Inst], pc: Int = 0, registers: Registers = Map.empty.withDefaultValue(0)):

    def terminated: Boolean =
      pc < 0 || pc >= prog.length

    def step: CPU =
      prog(pc) match
        case HLF(r)   => copy(pc = pc + 1, registers = registers.update(r, _ / 2))
        case TPL(r)   => copy(pc = pc + 1, registers = registers.update(r, _ * 3))
        case INC(r)   => copy(pc = pc + 1, registers = registers.update(r, _ + 1))
        case JMP(o)   => copy(pc = pc + o)
        case JIE(r,o) => if registers.even(r)  then copy(pc = pc + o) else copy(pc = pc + 1)
        case JIO(r,o) => if registers.isOne(r) then copy(pc = pc + o) else copy(pc = pc + 1)

    @tailrec
    final def run: CPU =
      if !terminated then step.run else this

  val start1: Long = System.currentTimeMillis
  val answer1: Int = CPU(instructions).run.registers.valueOf("b")
  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = CPU(instructions, registers = Map("a" -> 1).withDefaultValue(0)).run.registers.valueOf("b")
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
