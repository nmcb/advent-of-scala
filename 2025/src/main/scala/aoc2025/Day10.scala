package aoc2025

import nmcb.*
import nmcb.predef.*

import scala.annotation.*

object Day10 extends AoC:

  extension (i: Int)

    infix def **(that: Int): Int =
      math.pow(i, that).toInt

  type Lights = Long

  extension (lights: Lights)

    infix def toggle(b: Button): Lights =
      lights ^ b

    def asLightString(size: Int): String =
      @tailrec
      def loop(counter: Int, ls: Lights, result: String = ""): String =
        if ls <= 0 then
          s"[${result.leftPadTo(size, '.').reverse}]"
        else
          val display = if (ls & 1) == 1 then '#' else '.'
          val next    = ls >> 1
          loop(counter - 1, next, s"$display$result")
      loop(size, lights)

  object Lights:

    def fromString(s: String): Lights =
      s match
        case s"[$lights]" => lights.reverse.foldLeft(0): (ls, c) =>
          if c == '.' then ls << 1 else if c == '#' then (ls << 1) + 1 else sys.error(s"no light: $c")

  type Button = Long

  extension (buttons: Button)

    def indices: Vector[Int] =
      @tailrec
      def loop(bs: Button, count: Int = 0, result: Vector[Int] = Vector.empty): Vector[Int] =
        if bs <= 0 then
          result
        else
          if (bs & 1) == 1 then
            loop(bs >> 1, count + 1, result :+ count)
          else
            loop(bs >> 1, count + 1, result)
      loop(buttons)

    def asButtonString: String =
      @tailrec
      def loop(bs: Button, count: Int = 0, result: Vector[Int] = Vector.empty): String =
        if bs <= 0 then
          s"(${result.mkString(",")})"
        else
          if (bs & 1) == 1 then
            loop(bs >> 1, count + 1, result :+ count)
          else
            loop(bs >> 1, count + 1, result)
      loop(buttons)

  object Button:

    def fromString(s: String): Button =
      s match
        case s"($buttons)" => buttons.split(',').map(_.toInt).foldLeft(0)((bs,b) => bs + (2 ** b))

  extension (buttons: Vector[Button])

    def pressForLights: Lights =
      buttons.foldLeft(0L)(_ toggle _)


  case class Machine(lights: Lights, buttons: Vector[Button], joltages: Vector[Long]):

    def solve1: Int =
      Iterator
        .from(1)
        .flatMap(buttons.combinations)
        .findFirst(_.pressForLights == lights)
        .size

    /** credits https://github.com/sim642/adventofcode */
    def solve2: Long =

      // Meh... Z3
      import com.microsoft.z3.*
      import scala.jdk.CollectionConverters.*

      val ctx = new Context(Map("model" -> "true").asJava)
      import ctx._

      //        (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
      //
      //          x0  x1    x2  x3    x4    x5
      //          (3) (1,3) (2) (2,3) (0,2) (0,1)
      //
      //      i0:                     x4    x5    = 3
      //      i1:     x1                    x5    = 5
      //      i2:           x2  x3    x4          = 4
      //      i3: x0  x1        x3                = 7
      //
      //      x0 = 1, x1 = 3, x2 = 3, x4 = 1, x5 = 2


      val optimizer = mkOptimize()

      /** defines variables x0, x1, ... xn */
      val xs: Vector[IntExpr] =
        buttons
          .zipWithIndex
          .map((_,i) => mkIntConst(s"x$i"))

      /** add constraints x0 >= 0, x1 >= 0 ... xn >= 0 */
      for presses <- xs do
        optimizer.Add(mkGe(presses, mkInt(0)))

      /** add goal minimise for x0 + x1 + ... xn */
      val total: ArithExpr[IntSort] =
        xs.foldLeft[ArithExpr[IntSort]](mkInt(0)): (x,y) =>
          mkAdd(x,y)
      optimizer.MkMinimize(total)

      /** joltage(i) - b(i0) - b(i1) - b(in) = 0 */
      (buttons zip xs)
        .foldLeft(joltages.map[ArithExpr[IntSort]](mkInt)):
          case (expressions, (b, x)) =>
            b.indices.foldLeft(expressions): (expressions, i) =>
              expressions.updated(i, mkSub(expressions(i), x))
        .foreach: joltageLeft =>
          optimizer.Add(mkEq(joltageLeft, mkInt(0)))

      assert(optimizer.Check() == Status.SATISFIABLE)

      optimizer.getModel.evaluate(total, false).toString.toInt


  object Machine:

    def fromString(s: String): Machine =
      val components = s.split(" ").toVector
      val lights   = Lights.fromString(components.head)
      val buttons  = components.tail.init.map(Button.fromString)
      val joltages = components.last.tail.init.split(",").map(_.toLong).toVector
      Machine(lights, buttons, joltages)


  lazy val machines: Vector[Machine] = lines.map(Machine.fromString)

  lazy val answer1: Long = machines.map(_.solve1).sum
  lazy val answer2: Long = machines.map(_.solve2).sum
