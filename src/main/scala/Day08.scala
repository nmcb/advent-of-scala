import scala.annotation.tailrec
import scala.io.Source

object Day08 extends App:

  val day: String =
    this.getClass.getName.filter(_.isDigit).mkString("")

  type Registers = Map[String,Int]

  case class Inst(target: String, operation: Int => Int, condition: Registers => Boolean)

  case class CPU(instructions: List[Inst], registers: Registers, largestHeld: Int = 0):
    @tailrec
    final def run: CPU =
      instructions match
        case Nil =>
          this
        case inst :: rest =>
          CPU( instructions =
                 rest
             , registers =
                 if inst.condition(registers) then
                   registers.updatedWith(inst.target):
                     case None    => Some(inst.operation(0))
                     case Some(v) => Some(inst.operation(v))
                 else
                   registers
             , largestHeld =
                 if largestValue > largestHeld then
                   largestValue
                 else
                   largestHeld
          ).run

    def largestValue: Int =
      if registers.isEmpty then 0 else registers.valuesIterator.max

    def largestHeldValue: Int =
      largestHeld

  val cpu: CPU =
    val instructions: List[Inst] =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map:
          case s"$a $o $n if $b $c $w" =>
            val operation: Int => Int =
              o match
                case "inc" => r => r + n.toInt
                case "dec" => r => r - n.toInt
            val condition: Registers => Boolean =
              c match
                case "==" => rs => rs(b) == w.toInt
                case "!=" => rs => rs(b) != w.toInt
                case "<=" => rs => rs(b) <= w.toInt
                case ">=" => rs => rs(b) >= w.toInt
                case "<"  => rs => rs(b) <  w.toInt
                case ">"  => rs => rs(b) >  w.toInt
            Inst(a, operation, condition)
        .toList
    CPU(instructions, Map.empty.withDefaultValue(0))

  val start1: Long = System.currentTimeMillis
  val answer1: Int = cpu.run.largestValue
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = cpu.run.largestHeld
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
