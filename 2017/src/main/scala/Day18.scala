import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

object Day18 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Value = String | Long

  enum Inst:
    case SND(x: Value)
    case SET(x: String, y: Value)
    case ADD(x: String, y: Value)
    case MUL(x: String, y: Value)
    case MOD(x: String, y: Value)
    case RCV(x: String)
    case JGZ(x: Value, y: Value)

  import Inst.*

  val instructions: Vector[Inst] =
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

  case class Assembly(prog: Vector[Inst], pc: Long, mem: Map[String,Long], out: List[Long], in: List[Long]):

    def terminated: Boolean =
      pc < 0 || pc >= prog.size

    def awaiting: Boolean =
      in.isEmpty && instruction.isInstanceOf[RCV]

    private def instruction: Inst =
      prog(pc.toInt)

    extension (v: Value) def get: Long =
      v match
        case l: Long => l
        case r: String => mem(r)

    private def process(processRCV: String => Assembly => Assembly): Assembly =
      instruction match
        case SND(x)    => copy(pc = pc + 1, out = x.get :: out)
        case SET(x, y) => copy(pc = pc + 1, mem = mem + (x -> y.get))
        case ADD(x, y) => copy(pc = pc + 1, mem = mem + (x -> (x.get + y.get)))
        case MUL(x, y) => copy(pc = pc + 1, mem = mem + (x -> (x.get * y.get)))
        case MOD(x, y) => copy(pc = pc + 1, mem = mem + (x -> (x.get % y.get)))
        case RCV(x)    => processRCV(x)(this)
        case JGZ(x, y) => if x.get > 0 then copy(pc = pc + y.get) else copy(pc = pc + 1)

    @tailrec
    final def solve1: Assembly =
      if terminated || in.nonEmpty then
        this
      else
        process(x => _.copy(pc = pc + 1, mem = mem + (x -> out.head), in = out.head :: in)).solve1

    @tailrec
    final def solve2: Assembly =
      if terminated || awaiting  then
        this
      else
        process(x => _.copy(pc = pc + 1, mem = mem + (x -> in.last), in = in.init)).solve2


  object Assembly:
    def load(progId: Long, assembly: Vector[Inst]): Assembly =
      Assembly(
        prog = assembly,
        pc   = 0,
        mem  = Map.empty[String,Long].withDefaultValue(0L) + ("p" -> progId),
        out  = List.empty,
        in   = List.empty
      )

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = Assembly.load(0, instructions).solve1.in.last
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int =
    var assembly0 = Assembly.load(0, instructions)
    var assembly1 = Assembly.load(1, instructions)
    var count = 0
    while !(assembly0.terminated && assembly1.terminated) && !(assembly0.awaiting && assembly1.awaiting) do
      val next0 = assembly0.solve2
      val next1 = assembly1.solve2
      count = count + next1.out.size
      assembly0 = next0.copy(out = List.empty, in = next1.out ++ next0.in)
      assembly1 = next1.copy(out = List.empty, in = next0.out ++ next1.in)
    count
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
