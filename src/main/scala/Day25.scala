import scala.io.Source
import scala.util.Try

object Day25 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Value     = Int
  type Register  = String
  type Operand   = Value | Register
  type Registers = Map[Register, Value]

  extension (operand: Operand)
    // Shut up! I know what I'm doing.
    def isRegister: Boolean = operand.isInstanceOf[Register]
    def toRegister: Register = operand.asInstanceOf[Register]

  extension (registers: Registers)

    def valueOf(operand: Operand): Value =
      operand match
        case r: Register => registers.getOrElse(r, 0)
        case i: Int => i

    def update(register: Operand, operand: Operand, f: Value => Value = identity): Registers =
      if register.isRegister then
        val value = (f compose registers.valueOf)(operand)
        registers.updated(register.toRegister, value)
      else
        registers

  enum Instruction:
    case CPY(x: Operand, y: Operand)
    case INC(x: Operand)
    case DEC(x: Operand)
    case JNZ(x: Operand, y: Operand)
    case OUT(x: Operand)

  object Instruction:

    extension (s: String)
      def toOperand: Operand = Try(s.toInt).getOrElse(s)

    def line(s: String): Instruction =
      s match
        case s"cpy $x $y" => CPY(x.toOperand, y.toOperand)
        case s"inc $x"    => INC(x.toOperand)
        case s"dec $x"    => DEC(x.toOperand)
        case s"jnz $x $y" => JNZ(x.toOperand, y.toOperand)
        case s"out $x"    => OUT(x.toOperand)

  import Instruction.*

  enum State:
    case Run
    case Halted
    case Out(bit: Int)

  import State.*

  case class CPU(instructions: Vector[Instruction], pc: Int = 0, registers: Registers = Map.empty, state: State = Run):

    def halted: Boolean =
      pc >= instructions.size

    def step: CPU =
      if halted then
        copy(state = Halted)
      else
        instructions(pc) match
          case CPY(x, y)                              => copy(pc = pc + 1, registers = registers.update(y, x), state = Run)
          case INC(x)                                 => copy(pc = pc + 1, registers = registers.update(x, x, _ + 1), state = Run)
          case DEC(x)                                 => copy(pc = pc + 1, registers = registers.update(x, x, _ - 1), state = Run)
          case JNZ(x, y) if registers.valueOf(x) != 0 => copy(pc = pc + registers.valueOf(y), state = Run)
          case JNZ(x, y)                              => copy(pc = pc + 1, state = Run)
          case OUT(x)                                 => copy(pc = pc + 1, state = Out(registers.valueOf(x) & 0x1))

  val instructions: Vector[Instruction] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(line => Instruction.line(line))
      .toVector

  def experiment(instructions: Vector[Instruction], size: Int)(a: Int): String =
    Iterator.iterate(CPU(instructions, registers = Map("a" -> a)))(_.step)
      .collect:
        case CPU(_, _, _, Out(bit)) => bit
      .take(size)
      .mkString

  def solve1(instructions: Vector[Instruction]): Int =
    val sampleSize = 32
    val sample     = List.tabulate(sampleSize)(n => if n % 2 == 0 then '0' else '1').mkString
    Iterator.from(0).map(experiment(instructions, sampleSize)).indexWhere(_ == sample)

  val start1  = System.currentTimeMillis
  val answer1 = solve1(instructions)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2 = System.currentTimeMillis
  val answer2 = 50
  println(s"Answer day $day part 1: $answer2 stars [${System.currentTimeMillis - start2}ms]")
