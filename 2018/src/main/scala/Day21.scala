import scala.annotation.tailrec
import scala.io.Source

object Day21 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  type Codes = (Int, Int, Int, Int)

  extension (codes: Codes)
    def opcode: Int = codes._1
    def a: Int = codes._2
    def b: Int = codes._3
    def c: Int = codes._4

  type Mem   = Map[Int,Int]

  extension (mem: Mem)
    private def valueOf(reg: Int): Int                      = mem.getOrElse(reg, 0)
    def setRI(a: Int, b: Int, c: Int, f: Int => Int => Int) = mem.updated(c, f(mem.valueOf(a))(b))
    def setIR(a: Int, b: Int, c: Int, f: Int => Int => Int) = mem.updated(c, f(a)(mem.valueOf(b)))
    def setRR(a: Int, b: Int, c: Int, f: Int => Int => Int) = mem.updated(c, f(mem.valueOf(a))(mem.valueOf(b)))
    def setRN(a: Int, c: Int)                               = mem.updated(c, mem.valueOf(a))
    def setIN(a: Int, c: Int)                               = mem.updated(c, a)

  enum Inst:
    case ADDR(a: Int, b: Int, c: Int)
    case ADDI(a: Int, b: Int, c: Int)
    case MULR(a: Int, b: Int, c: Int)
    case MULI(a: Int, b: Int, c: Int)
    case BANR(a: Int, b: Int, c: Int)
    case BANI(a: Int, b: Int, c: Int)
    case BORR(a: Int, b: Int, c: Int)
    case BORI(a: Int, b: Int, c: Int)
    case SETR(a: Int, c: Int)
    case SETI(a: Int, c: Int)
    case GTIR(a: Int, b: Int, c: Int)
    case GTRI(a: Int, b: Int, c: Int)
    case GTRR(a: Int, b: Int, c: Int)
    case EQIR(a: Int, b: Int, c: Int)
    case EQRI(a: Int, b: Int, c: Int)
    case EQRR(a: Int, b: Int, c: Int)

    def execute(mem: Mem): Mem =
      this match
        case ADDR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a + b)
        case ADDI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a + b)
        case MULR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a * b)
        case MULI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a * b)
        case BANR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a & b)
        case BANI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a & b)
        case BORR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => a | b)
        case BORI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => a | b)
        case SETR(a, c)    => mem.setRN(a, c)
        case SETI(a, c)    => mem.setIN(a, c)
        case GTIR(a, b, c) => mem.setIR(a, b, c, (a: Int) => (b: Int) => if a  > b then 1 else 0)
        case GTRI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => if a  > b then 1 else 0)
        case GTRR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => if a  > b then 1 else 0)
        case EQIR(a, b, c) => mem.setIR(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)
        case EQRI(a, b, c) => mem.setRI(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)
        case EQRR(a, b, c) => mem.setRR(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)

  import Inst.*

  var i = 0
  case class CPU(pragma: Int, program: Vector[Inst], pc: Int = 0, mem: Mem = Map.empty):

    def halted: Boolean = !program.indices.contains(pc)

    def execute(inst: Inst): CPU =
      val bind   = this.copy(mem = this.mem.setIN(this.pc, this.pragma))
      val exec   = bind.copy(mem = inst.execute(bind.mem))
      val result = exec.copy(pc  = exec.mem.valueOf(exec.pragma) + 1)
      result

    @tailrec
    final def nextAfterEQRR: CPU =
      if halted then
        this
      else if program(pc).isInstanceOf[EQRR] then
        execute(program(pc))
      else
        execute(program(pc)).nextAfterEQRR

  val cpu: CPU =
    val lines = Source.fromResource(s"input$day.txt").getLines.toVector

    val pragma = lines(0) match
      case s"#ip $r" => r.toInt

    val program = lines.collect:
      case s"$inst $a $b $c" => inst.toUpperCase match
        case "ADDR" => ADDR(a.toInt, b.toInt, c.toInt)
        case "ADDI" => ADDI(a.toInt, b.toInt, c.toInt)
        case "MULR" => MULR(a.toInt, b.toInt, c.toInt)
        case "MULI" => MULI(a.toInt, b.toInt, c.toInt)
        case "BANR" => BANR(a.toInt, b.toInt, c.toInt)
        case "BANI" => BANI(a.toInt, b.toInt, c.toInt)
        case "BORR" => BORR(a.toInt, b.toInt, c.toInt)
        case "BORI" => BORI(a.toInt, b.toInt, c.toInt)
        case "SETR" => SETR(a.toInt,          c.toInt)
        case "SETI" => SETI(a.toInt,          c.toInt)
        case "GTIR" => GTIR(a.toInt, b.toInt, c.toInt)
        case "GTRI" => GTRI(a.toInt, b.toInt, c.toInt)
        case "GTRR" => GTRR(a.toInt, b.toInt, c.toInt)
        case "EQIR" => EQIR(a.toInt, b.toInt, c.toInt)
        case "EQRI" => EQRI(a.toInt, b.toInt, c.toInt)
        case "EQRR" => EQRR(a.toInt, b.toInt, c.toInt)

    CPU(pragma, program)

  /**
   * @see Credits - https://www.reddit.com/r/adventofcode/comments/a86jgt/comment/ec8fnq8
   *
   * Realizing that the only time register 0 was being used was on line 28, in:
   *
   * 28: eqrr 5 0 1
   *
   * Together with the rest of the program doesn't contain an eqrr instruction,
   * we just check what register 5 contains the first time the line is executed.
   */
  val start1  = System.currentTimeMillis
  val answer1 = cpu.nextAfterEQRR.mem.valueOf(5)
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  /**
   * For part 2, we check when the value in register 5 repeats. When it does,
   * the last value must have been the one that causes the most instructions.
   * The solution takes about 2 minutes so the result is copied in.
   */
  val start2  = System.currentTimeMillis
  val answer2 = 5876609 // Day18.Cycle.find(cpu)(_.nextAfterEQRR, _.mem.valueOf(5)).cycleLast.mem.valueOf(5)
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
