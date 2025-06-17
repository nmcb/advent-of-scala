import Day23.Instruction.CPY

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day23 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  type Value     = Int
  type Register  = String
  type Operand   = Value | Register
  type Registers = Map[Register, Value]

  extension (operand: Operand)
    // Shut up! I know what I'm doing.
    def isRegister: Boolean  = operand.isInstanceOf[Register]
    def toRegister: Register = operand.asInstanceOf[Register]

  extension (registers: Registers)

    def valueOf(operand: Operand): Value =
      operand match
        case r: Register => registers.getOrElse(r, 0)
        case i: Int      => i

    def update(register: Operand, operand: Operand, f: Value => Value = identity): Registers =
      if register.isRegister then
        val value = (f compose registers.valueOf)(operand)
        registers.updated(register.toRegister, value)
      else
        registers

    // Part 2
    def multiply(x: Operand, y: Operand, z: Operand): Registers =
      val value = registers.valueOf(x) * registers.valueOf(y) + registers.valueOf(z)
      registers.updated(z.toRegister, value)


  enum Instruction:
    case CPY(x: Operand, y: Operand)
    case INC(x: Operand)
    case DEC(x: Operand)
    case JNZ(x: Operand, y: Operand)
    case TGL(x: Operand)
    // Part 2
    case MUL(x: Operand, y: Operand, z: Operand)

  object Instruction:

    extension (s: String)
      def toOperand: Operand   = Try(s.toInt).getOrElse(s)

    def line(s: String): Instruction =
      s match
        case s"cpy $x $y"    => CPY(x.toOperand, y.toOperand)
        case s"inc $x"       => INC(x.toOperand)
        case s"dec $x"       => DEC(x.toOperand)
        case s"jnz $x $y"    => JNZ(x.toOperand, y.toOperand)
        case s"tgl $x"       => TGL(x.toOperand)
        // Part 2
        case s"mul $x $y $z" => MUL(x.toOperand, y.toOperand, z.toOperand)

  var count = 0;

  case class CPU(instructions: Vector[Option[Instruction]], pc: Int = 0, registers: Registers = Map.empty):

    import Instruction.*

    def halted: Boolean =
      pc >= instructions.size

    extension (instructions: Vector[Option[Instruction]])
      def toggle(offset: Operand): Vector[Option[Instruction]] =
        val index = pc + registers.valueOf(offset)
        if instructions.indices.contains(index) then
          instructions.updated(index, instructions(index) match
            case None            => None
            case Some(CPY(o, r)) => Some(JNZ(o, r))
            case Some(INC(r))    => Some(DEC(r))
            case Some(DEC(r))    => Some(INC(r))
            case Some(JNZ(o, v)) => Some(CPY(o, v))
            case Some(TGL(x))    => Some(INC(x))
            //Part 2
            case _               => sys.error("boom!")
          )
        else
          instructions

    @tailrec
    final def run: CPU =
      if halted then
        this
      else
        instructions(pc) match
          case None =>
            copy(pc = pc + 1).run
          case Some(CPY(x, y)) =>
            copy(pc = pc + 1, registers = registers.update(y, x)).run
          case Some(INC(x)) =>
            copy(pc = pc + 1, registers = registers.update(x, x, _ + 1)).run
          case Some(DEC(x)) =>
            copy(pc = pc + 1, registers = registers.update(x, x, _ - 1)).run
          case Some(JNZ(x, y)) if registers.valueOf(x) != 0 =>
            copy(pc = pc + registers.valueOf(y)).run
          case Some(JNZ(x, y)) =>
            copy(pc = pc + 1).run
          case Some(TGL(x)) =>
            copy(pc = pc + 1, instructions = instructions.toggle(x)).run
          case Some(MUL(x, y, z)) =>
            copy(pc = pc + 1, registers = registers.multiply(x, y, z)).run

  def instructions(patched: Boolean = false): Vector[Option[Instruction]] =
    Source
      .fromResource(if patched then s"input$day-patched.txt" else s"input$day.txt")
      .getLines
      .map(line => Some(Instruction.line(line)))
      .toVector


  val start1  = System.currentTimeMillis
  val answer1 = CPU(instructions(), registers = Map("a" -> 7)).run.registers.valueOf("a")
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  /**
   * The problem strongly hints on missing a multiplication. Inspecting the assembly contains:
   *
   * cpy b c
   * inc a
   * dec c
   * jnz c -2
   * dec d
   * jnz d -5
   *
   * Which adds b times d to a while clearing c and d. Replace that block of code with a same
   * sized block of code that contains a handcrafted three argument multiplication statement and
   * take a leap of faith that the tgl c instruction later on will not mess up this patch it might
   * work. The patch looks like this:
   *
   * mul b d a
   * cpy 0 c
   * cpy 0 c
   * cpy 0 c
   * cpy 0 c
   * cpy 0 d
   *
   */

  val start2  = System.currentTimeMillis
  val answer2 = CPU(instructions(patched = true), registers = Map("a" -> 12)).run.registers.valueOf("a")
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
