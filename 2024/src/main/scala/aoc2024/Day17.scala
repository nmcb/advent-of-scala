package aoc2024

import nmcb.*
import nmcb.predef.*

import scala.annotation.*

object Day17 extends AoC:

  case class CPU(a: Long, b: Long, c: Long, program: Vector[Long], ip: Int = 0, out: Long = 0):

    def opcode: Long =
      program(ip)

    def operand: Long =
      program(ip + 1)

    def halts: Boolean =
      ip >= program.size

    @tailrec
    final def run: CPU =
      if halts then this else read.run

    def combo: Long =
      operand match
        case 0 | 1 | 2 | 3 => operand
        case 4 => a
        case 5 => b
        case 6 => c
        case _ => sys.error(s"Invalid combo operand: $operand")

    def literal: Long =
      operand

    def read: CPU =
      opcode match
        case 0 => copy(a = a >> combo, ip = ip + 2)
        case 1 => copy(b = b ^ literal, ip = ip + 2)
        case 2 => copy(b = combo & 0x07, ip = ip + 2)
        case 3 => copy(ip = if a != 0 then literal.toInt else ip + 2)
        case 4 => copy(b = b ^ c, ip = ip + 2)
        case 5 => copy(out = (out << 3) + (combo & 0x07), ip = ip + 2)
        case 6 => copy(b = a >> combo, ip = ip + 2)
        case 7 => copy(c = a >> combo, ip = ip + 2)
        case _ => sys.error(s"Invalid r.performed: ip=$ip, inst=${program(ip)}")

    @tailrec
    final def display(current: Long, result: String = ""): String =
      if current == 0 then
        Option.when(result != "")(result).getOrElse("0")
      else
        display(current >> 3, (current & 0x7).toString + result)

    def displayOut: String =
      display(out)

  val cpu: CPU =
    
    val source =
      lines
        .collect:
          case s"$p: $v" => p -> v.trim.split(',').map(_.toLong).toVector
        .toMap

    CPU(
      a = source("Register A").head,
      b = source("Register B").head,
      c = source("Register C").head,
      program = source("Program")
    )

  def quineA(cpu: CPU): Long =
    @tailrec
    def loop(search: String, digits: Int, a: Long): Long =
      val next = cpu.copy(a = a).run
      val out = next.displayOut.leftPadTo(digits, '0')

      if out == search then a
      else if out == search.takeRight(digits) then loop(search, digits + 1, a << 3)
      else loop(search, digits, a + 1)

    val program: String = cpu.program.mkString("")
    val search = program.leftPadTo(program.length, '0')
    loop(search, 1, 0)

  lazy val answer1: String = cpu.run.displayOut.mkString(",")
  lazy val answer2: Long = quineA(cpu)
