
import nmcb.predef.*
import scala.io.Source
import scala.util.Try

object Day23 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type Value = String | Long

  enum Inst:
    case SET(x: String, y: Value)
    case SUB(x: String, y: Value)
    case MUL(x: String, y: Value)
    case JNZ(x: Value, y: Value)

  import Inst.*

  val instructions: Vector[Inst] =
    def parse(s: String): Value = Try(s.toLong).getOrElse(s)

    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map:
        case s"set $x $y" => SET(x, parse(y))
        case s"sub $x $y" => SUB(x, parse(y))
        case s"mul $x $y" => MUL(x, parse(y))
        case s"jnz $x $y" => JNZ(parse(x), parse(y))
      .toVector

  case class CPU(prog: Vector[Inst], mem: Map[String,Long], pc: Int, countMUL: Long = 0):

    extension (v: Value) def get: Long =
      v match
        case l: Long   => l
        case r: String => mem(r)

    def running: Boolean =
      pc >= 0 && pc < prog.size

    def step: CPU =
      if running then
        prog(pc) match
          case SET(x,y) => copy(mem = mem + (x -> y.get), pc = pc + 1)
          case SUB(x,y) => copy(mem = mem + (x -> (x.get - y.get)), pc = pc + 1)
          case MUL(x,y) => copy(mem = mem + (x -> (x.get * y.get)), pc = pc + 1, countMUL = countMUL + 1)
          case JNZ(x,y) => copy(pc = if x.get != 0 then pc + y.get.toInt else pc + 1)
      else
        this

  object CPU:
    def load(prog: Vector[Inst]): CPU =
      CPU(
        prog = prog,
        mem  = Map.empty.withDefaultValue(0L),
        pc   = 0
      )

  def solve(prog: Vector[Inst]): Long =
    Iterator.iterate(CPU.load(prog))(_.step).findFirstNot(_.running).countMUL

  val start1: Long  = System.currentTimeMillis
  lazy val answer1: Long = solve(instructions)
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  /*

  --Instructions

  set b 57
  set c b
  jnz a 2
  jnz 1 5
  mul b 100
  sub b -100000
  set c b
  sub c -17000
  set f 1
  set d 2
  set e 2
  set g d
  mul g e
  sub g b
  jnz g 2
  set f 0
  sub e -1
  set g e
  sub g b
  jnz g -8
  sub d -1
  set g d
  sub g b
  jnz g -13
  jnz f 2
  sub h -1
  set g b
  sub g c
  jnz g 2
  jnz 1 3
  sub b -17
  jnz 1 -23


  --Loop and if identification

  set b 57
  set c b

  jnz a 2
  jnz 1 5
  mul b 100
  sub b -100000
  set c b
  sub c -17000

    set f 1
    set d 2
      set e 2
        set g d
        mul g e
        sub g b

        jnz g 2
        set f 0

        sub e -1
        set g e
        sub g b
        jnz g -8
      sub d -1
      set g d
      sub g b
      jnz g -13

    jnz f 2
    sub h -1

    set g b
    sub g c

    jnz g 2
    jnz 1 3
    sub b -17
    jnz 1 -23


  --Higher level reengineering

  c = b = 57

  if a != 0:
    b = b * 100 + 100000
    c = b + 17000

  #b = 105700
  #c = 122700

    f = 1
    d = 2
      e = 2
        g = d * e - b

        if g == 0:
          f = 0


        e += 1
        g = e - b
        if g != 0: loop
      d += 1
      g = d - b
      if g != 0 loop

    if f == 0
      h += 1

    g = b - c
    if g == 0: break
    b += 17
    loop


  --Pseudo code reconstruction

  var h = 0

  for
    b <- 105700 to 122700 by 17
  do
    var f = 1

    for
      d <- 2 to b + 1
      e <- 2 to b + 1
    do
      if d * e == b then f = 0

    if f == 0 then h += 1


  --Prime recognition

  var h = 0

  for
    b <- 105700 to 122700 by 17
  do
    if !prime(b) h += 1

  */


  def notPrime(n: Int): Boolean =
    (2 to math.sqrt(n.toDouble).toInt).exists(n % _ == 0)

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = (105700 to 122700 by 17).count(notPrime)
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
