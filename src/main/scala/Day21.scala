import scala.annotation.targetName
import scala.io.*

object Day21 extends App:

  val day: String = this.getClass.getName.drop(3).init

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
          case (ro: Lazy, ll: Long) => ro.compose(rl => ll + rl)
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

  sealed abstract class Expr(val name: Name)
    extends ((Name => Res) => Res)

  case class Val(override val name: Name, num: Long)
    extends Expr(name):
      def apply(f: Name => Res): Res = num

  case class Bin(override val name: Name, lhs: Name, rhs: Name, op: String)
    extends Expr(name):
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
        case s"$n: $l $o $r" => Bin(n, l, r, o)
        case s"$n: $v"       => Val(n, v.toInt)

  case class SAT(input: List[Expr]):
    private def loop(name: Name): Res =
      input.find(_.name == name).map(e => e(loop)).getOrElse(identity)

    def solve: Long =
      loop("root") match
        case l: Long => l
        case _: Lazy => sys.error("unsolvable")

  val program: List[Expr] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(Expr.parseLine)
      .toList

  val start1: Long  = System.currentTimeMillis
  val answer1 = SAT(program).solve
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val patch = program.flatMap {
    case Bin("root", lhs, rhs, _) => Some(Bin("root", lhs, rhs, "="))
    case m if m.name == "humn"    => None
    case m                        => Some(m)
  }

  val start2: Long  = System.currentTimeMillis
  val answer2 = SAT(patch).solve
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")

