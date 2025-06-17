import scala.annotation.*
import scala.io.*

object Day07 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  /** Modeling */
  
  type Wire = String
  type Env  = Map[Wire,Int]

  sealed trait Rule:
    def args: List[Wire]
    def ret: Wire
    def run(env: Env): Option[Int]

  case class Op2(op: Int => Int => Int, args: List[Wire], ret: Wire) extends Rule:

    def run(env: Env): Option[Int] =
      val List(lhs, rhs) = args.map(env.get)
      for {
        v1 <- lhs
        v2 <- rhs
      } yield op(v1)(v2)

  case class Op1(op: Int => Int, args: List[Wire], ret: Wire) extends Rule:

    def run(env: Env): Option[Int] =
      args.map(env.get).headOption.flatten.map(op)

  case class Val(value: Int, ret: Wire) extends Rule:

    def args: List[Wire] =
      List.empty

    def run(env: Env): Option[Int] =
      Some(value)

  object Solver:
    def solve(rules: Seq[Rule], wire: Wire, setWireB: Option[Int] = None): Int =

      @tailrec def fold(rules: Seq[Rule], env: Env = Map.empty): Int =
        env.get(wire) match
          case Some(v) => v
          case None    => rules match
            case Seq(rule, rest*) => rule.run(env) match
              case Some(v)        => fold(rest, env.updated(rule.ret, v))
              case None           => fold(rest :+ rule, env)
            case _                => sys.error(s"undefined wire=$wire")

      val puzzleInput: Seq[Rule] = setWireB.map(v => Val(v, "b") +: rules.filterNot(_.ret == "b")).getOrElse(rules)
      fold(puzzleInput)

  val input: IndexedSeq[Rule] =
    def parser(s: Wire): Rule =
      s match
        case s"$lhs AND $rhs -> $ret" if lhs.toIntOption.isDefined
          => Op1(rv => lhs.toInt & rv, List(rhs), ret)
        case s"$lhs AND $rhs -> $ret"
          => Op2(lv => rv => lv & rv, List(lhs, rhs), ret)
        case s"$lhs OR $rhs -> $ret"
          => Op2(lv => rv => lv | rv, List(lhs, rhs), ret)
        case s"$lhs RSHIFT $rhs -> $ret" if rhs.toIntOption.isDefined
          => Op1(lv => lv >> rhs.toInt, List(lhs), ret)
        case s"$lhs RSHIFT $rhs -> $ret"
          => Op2(lv => rv => lv >> rv, List(lhs, rhs), ret)
        case s"$lhs LSHIFT $rhs -> $ret" if rhs.toIntOption.isDefined
          => Op1(lv => lv << rhs.toInt, List(lhs), ret)
        case s"$lhs LSHIFT $rhs -> $ret"
          => Op2(lv => rv => lv << rv, List(lhs, rhs), ret)
        case s"NOT $rhs -> $ret"
          => Op1(rv => ~rv & 0x0000FFFF, List(rhs), ret)
        case s"$arg -> $ret" if arg.toIntOption.isDefined
          => Val(arg.toInt, ret)
        case s"$rhs -> $ret"
          => Op1(identity, List(rhs), ret)

    Source
      .fromResource(s"input$day.txt", getClass.getClassLoader)
      .getLines
      .map(parser)
      .toIndexedSeq


  /** Part 1 */

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Solver.solve(rules = input, wire = "a")
  println(s"Answer AOC 2015 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Solver.solve(rules = input, wire = "a", setWireB = Some(answer1))
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
