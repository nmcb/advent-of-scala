import scala.annotation.tailrec
import scala.io.Source

object Day16 extends App:

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
    case ADDR
    case ADDI
    case MULR
    case MULI
    case BANR
    case BANI
    case BORR
    case BORI
    case SETR
    case SETI
    case GTIR
    case GTRI
    case GTRR
    case EQIR
    case EQRI
    case EQRR

    def execute(codes: Codes)(mem: Mem): Mem =
      val (_, a, b, c) = codes
      this match
        case ADDR => mem.setRR(a, b, c, (a: Int) => (b: Int) => a + b)
        case ADDI => mem.setRI(a, b, c, (a: Int) => (b: Int) => a + b)
        case MULR => mem.setRR(a, b, c, (a: Int) => (b: Int) => a * b)
        case MULI => mem.setRI(a, b, c, (a: Int) => (b: Int) => a * b)
        case BANR => mem.setRR(a, b, c, (a: Int) => (b: Int) => a & b)
        case BANI => mem.setRI(a, b, c, (a: Int) => (b: Int) => a & b)
        case BORR => mem.setRR(a, b, c, (a: Int) => (b: Int) => a | b)
        case BORI => mem.setRI(a, b, c, (a: Int) => (b: Int) => a | b)
        case SETR => mem.setRN(a, c)
        case SETI => mem.setIN(a, c)
        case GTIR => mem.setIR(a, b, c, (a: Int) => (b: Int) => if a > b then 1 else 0)
        case GTRI => mem.setRI(a, b, c, (a: Int) => (b: Int) => if a > b then 1 else 0)
        case GTRR => mem.setRR(a, b, c, (a: Int) => (b: Int) => if a > b then 1 else 0)
        case EQIR => mem.setIR(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)
        case EQRI => mem.setRI(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)
        case EQRR => mem.setRR(a, b, c, (a: Int) => (b: Int) => if a == b then 1 else 0)

  type Test  = (Mem,Codes,Mem)

  extension (test: Test)
    def before: Mem  = test._1
    def codes: Codes = test._2
    def after: Mem   = test._3

  val (tests: Vector[Test], program: Vector[Codes]) =
    val Array(prefix, postfix) =
      Source
        .fromResource(s"input$day.txt")
        .mkString
        .split("\n\n\n")

    val tests =
      prefix
        .linesIterator
        .filterNot(_.isBlank)
        .grouped(3)
        .map: lines =>
          val before = lines(0) match
            case s"Before: [$r0, $r1, $r2, $r3]" =>
              Map(0 -> r0.toInt, 1 -> r1.toInt, 2 -> r2.toInt, 3 -> r3.toInt)
          val instruction = lines(1) match
            case s"$op $a $b $c" =>
              (op.toInt, a.toInt, b.toInt, c.toInt)
          val after = lines(2) match
            case s"After:  [$r0, $r1, $r2, $r3]" =>
              Map(0 -> r0.toInt, 1 -> r1.toInt, 2 -> r2.toInt, 3 -> r3.toInt)
          (before, instruction, after)
        .toVector

    val codes =
      postfix
        .linesIterator
        .filterNot(_.isBlank)
        .map:
          case s"$op $a $b $c" =>
            (op.toInt, a.toInt, b.toInt, c.toInt)
        .toVector

    (tests, codes)

  def comply(test: Test): Set[Inst] =
    val (before, codes, after) = test
    Inst.values.filter(_.execute(codes)(before) == after).toSet

  val start1  = System.currentTimeMillis
  val answer1 = tests.map(comply).count(_.size >= 3)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def reengineer(tests: Vector[Test]): Map[Int,Inst] =
    @tailrec
    def go(todo: Map[Int,Set[Inst]], found: Map[Int,Inst] = Map.empty): Map[Int,Inst] =

      type Todo = (Int,Set[Inst])

      extension (todo: Todo)
        def opcode: Int           = todo._1
        def candidates: Set[Inst] = todo._2
        def instruction: Inst     = candidates.head

      todo.find(_.candidates.size == 1) match
        case None =>
          found
        case Some(result) =>
          go(
            todo  = todo.view.mapValues(_ - result.instruction).toMap - result.opcode,
            found = found + (result.opcode -> result.instruction)
          )

    go(tests.groupMapReduce(_.codes.opcode)(comply)(_ union _))

  def solve2(tests: Vector[Test], program: Vector[Codes]): Int =
    val instructionOf: Map[Int,Inst] = reengineer(tests)
    program.foldLeft(Map.empty[Int,Int])((mem,codes) => instructionOf(codes.opcode).execute(codes)(mem))(0)

  val start2  = System.currentTimeMillis
  val answer2 = solve2(tests, program)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
