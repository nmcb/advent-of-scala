import scala.annotation.targetName
import scala.io.*

object Day21 extends App:

  val day: String = this.getClass.getName.drop(3).init

  @targetName("peaceBeUponHim")
  def ﷺ[A](a: A): A =
    a

  val peaceBeUponHim = ﷺ

  type Name = String
  type Lazy = Long => Long
  type Res  = Long | Lazy

  object Res:
    extension (lhs: Res)

      @targetName("add")
      def |+|(rhs: Res): Res =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => lo.andThen(ro)
          case (ll: Long, ro: Lazy) => ro.compose(rl => rl - ll)
          case (lo: Lazy, rl: Long) => lo.compose(ll => ll - rl)
          case (ll: Long, rl: Long) => ll + rl

      @targetName("mul")
      def |*|(rhs: Res): Res =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => lo.andThen(ro)
          case (ll: Long, ro: Lazy) => ro.compose(rl => rl / ll)
          case (lo: Lazy, rl: Long) => lo.compose(ll => ll / rl)
          case (ll: Long, rl: Long) => ll * rl

      @targetName("sub")
      def |-|(rhs: Res): Res =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => lo.andThen(ro)
          case (ll: Long, ro: Lazy) => ro.compose(rl => ll - rl)
          case (lo: Lazy, rl: Long) => lo.compose(ll => rl + ll)
          case (ll: Long, rl: Long) => ll - rl

      @targetName("div")
      def |/|(rhs: Res): Res =
        (lhs, rhs) match
          case (lo: Lazy, ro: Lazy) => ro.andThen(lo)
          case (ll: Long, ro: Lazy) => ro.compose(rl => ll / rl)
          case (lo: Lazy, rl: Long) => lo.compose(ll => rl * ll)
          case (ll: Long, rl: Long) => ll / rl

      @targetName("eq")
      def |=|(rhs: Res): Res =
        (lhs, rhs) match
          case (ll: Long, ro: Lazy)             => ro(ll)
          case (lo: Lazy, rl: Long)             => lo(rl)
          case (ll: Long, rl: Long) if rl == ll => rl
          case _ => sys.error("boom!")

  sealed abstract class Expr(val name: Name, line: String)
    extends ((Name => Res) => Res):
      override def toString: String = line + "\n"

  case class Val(override val name: Name, num: Long, line: String)
    extends Expr(name, line):
      def apply(defer: Name => Res): Res = num

  case class Bin(override val name: Name, lhs: Name, rhs: Name, op: String, line: String)
    extends Expr(name, line):
      import Res.*
      def apply(defer: Name => Res): Res =
        op match
          case "+" => defer(lhs) |+| defer(rhs)
          case "*" => defer(lhs) |*| defer(rhs)
          case "-" => defer(lhs) |-| defer(rhs)
          case "/" => defer(lhs) |/| defer(rhs)
          case "=" => defer(lhs) |=| defer(rhs)

  object Expr:
    def parseLine(line: String): Expr =
      line match
        case s"$n: $l $o $r" => Bin(n, l, r, o, s"$n: $l $o $r")
        case s"$n: $v"       => Val(n, v.toInt, s"$n: $v")

  case class SAT(input: List[Expr]):
    private val search: Name => Res =
      name =>
        input
          .find(_.name == name)
          .map(e => e.apply(search))
          .getOrElse(ﷺ)

    val solve: Option[Long] =
      search("root") match
        case l: Long => Some(l)
        case _: Lazy => None

  val program: List[Expr] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Expr.parseLine)
      .toList

  val start1: Long  = System.currentTimeMillis
  val answer1 = SAT(program).solve.getOrElse(sys.error(s"unsolved ${program.foldLeft("\n")(_ + _)}"))
  println(s"Answer AOC 2022 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val patch = program.flatMap {
    case Bin("root", lhs, rhs, op, line) => Some(Bin("root", lhs, rhs, "=", line.replace(op.head, '=')))
    case m if m.name == "humn"    => None
    case m                        => Some(m)
  }

  val start2: Long  = System.currentTimeMillis
  val answer2 = SAT(patch).solve.getOrElse(sys.error(s"unsolved ${program.foldLeft("\n")(_ + _)}"))
  println(s"Answer AOC 2022 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

