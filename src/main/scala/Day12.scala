import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day12 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Value     = Int
  type Register  = String
  type Operand   = Value | Register
  type Registers = Map[Register, Value]

  extension (registers: Registers)

    def valueOf(operand: Operand): Value =
      operand match
        case r: Register => registers.getOrElse(r, 0)
        case i: Int      => i

    def update(register: Register, operand: Operand, f: Value => Value = identity): Registers =
      val value = (f compose registers.valueOf)(operand)
      registers.updated(register, value)


  enum Instruction:
    case CPY(operand: Operand, register: Register)
    case INC(register: Register)
    case DEC(register: Register)
    case JNZ(operand: Operand, offset: Value)

  object Instruction:

    extension (s: String)
      def toValue: Value       = s.toInt
      def toRegister: Register = s
      def toOperand: Operand   = Try(s.toValue).getOrElse(s)


    def fromString(s: String): Instruction =
      s match
        case s"cpy $x $y" => CPY(x.toOperand, y.toRegister)
        case s"inc $x"    => INC(x.toRegister)
        case s"dec $x"    => DEC(x.toRegister)
        case s"jnz $x $y" => JNZ(x.toOperand, y.toValue)


  case class CPU(instructions: Vector[Instruction], pc: Int = 0, registers: Registers = Map.empty):

    import Instruction.*

    def halted: Boolean =
      pc >= instructions.size

    @tailrec
    final def run: CPU =
      if halted then
        this
      else
        instructions(pc) match
          case CPY(o, r) => copy(pc = pc + 1, registers = registers.update(r, o)).run
          case INC(r)    => copy(pc = pc + 1, registers = registers.update(r, r, _ + 1)).run
          case DEC(r)    => copy(pc = pc + 1, registers = registers.update(r, r, _ - 1)).run
          case JNZ(o, v) => if registers.valueOf(o) != 0 then copy(pc = pc + v).run else copy(pc = pc + 1).run

  lazy val instructions: Vector[Instruction] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Instruction.fromString)
      .toVector

  val start1  = System.currentTimeMillis
  val answer1 = CPU(instructions).run.registers.valueOf("a")
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  val answer2 = CPU(instructions = instructions, registers = Map("c" -> 1)).run.registers.valueOf("a")
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
