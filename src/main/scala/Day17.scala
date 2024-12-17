import scala.io.*
import scala.annotation.*

import nmcb.*
import Extensions.*

object Day17 extends App:

  val day: String =
    getClass.getName.drop(3).init

  sealed abstract class Inst(cpu: CPU):
    def operand: Long
    def perform: CPU

    def literal: Long =
      operand

    def combo: Long =
      operand match
        case 0 | 1 | 2 | 3 => operand
        case 4 => cpu.a
        case 5 => cpu.b
        case 6 => cpu.c
        case _ => sys.error(s"Invalid combo operand: $operand")

  case class ADV(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(a = cpu.a >> combo, ip = cpu.ip + 2)

  case class BXL(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(b = cpu.b ^ literal, ip = cpu.ip + 2)

  case class BST(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(b = combo & 0x07, ip = cpu.ip + 2)

  case class JNZ(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = if cpu.a != 0 then cpu.copy(ip = literal.toInt) else cpu.copy(ip = cpu.ip + 2)

  case class BXC(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(b = cpu.b ^ cpu.c, ip = cpu.ip + 2)

  case class OUT(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(out = (cpu.out << 3) + (combo & 0x07), ip = cpu.ip + 2)

  case class BDV(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(b = cpu.a >> combo, ip = cpu.ip + 2)

  case class CDV(override val operand: Long, cpu: CPU) extends Inst(cpu):
    def perform: CPU = cpu.copy(c = cpu.a >> combo, ip = cpu.ip + 2)


  case class CPU(a: Long, b: Long, c: Long, program: Vector[Long], ip: Int = 0, out: Long = 0):
    lazy val opcode  = program(ip)
    lazy val operand = program(ip + 1)

    val halts: Boolean =
      ip >= program.size

    @tailrec
    final def run: CPU =
      if halts then this else read.perform.run

    def read: Inst =
      opcode match
        case 0 => ADV(operand, this)
        case 1 => BXL(operand, this)
        case 2 => BST(operand, this)
        case 3 => JNZ(operand, this)
        case 4 => BXC(operand, this)
        case 5 => OUT(operand, this)
        case 6 => BDV(operand, this)
        case 7 => CDV(operand, this)
        case _ => sys.error(s"Invalid read: ip=$ip, inst=${program(ip)}")

    @tailrec
    final def display(current: Long, result: String = ""): String =
      if current == 0 then
        Option.when(result != "")(result).getOrElse("0")
      else
        display(current >> 3, (current & 0x7).toString + result)

    def displayOut: String =
      display(out)

    def quineA: Long =
      @tailrec
      def loop(search: String, digits: Int, a: Long): Long =
        val next = cpu.copy(a = a).run
        val out  = next.displayOut.leftPadTo(digits, '0')

        if out == search                        then a
        else if out == search.takeRight(digits) then loop(search, digits + 1, a << 3)
        else                                         loop(search, digits, a + 1)

      val program: String = cpu.program.mkString("")
      val search = program.leftPadTo(program.length, '0')
      loop(search, 1, 0)

  val cpu: CPU =
    val input =
      Source.fromResource(s"input$day.txt").getLines
        .collect:
          case s"$p: $v" => p -> v.trim.split(',').map(_.toLong).toVector
        .toMap

    CPU(
      a = input("Register A").head,
      b = input("Register B").head,
      c = input("Register C").head,
      program = input("Program")
    )


  val start1: Long    = System.currentTimeMillis
  val answer1: String = cpu.run.displayOut.mkString(",")
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = cpu.quineA
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
