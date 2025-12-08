package aoc2021

import nmcb.*
import scala.collection.*

object Day24 extends AoC:

  case class Step(a: Int, b: Int, d: Int)

  case class Constraint(delta: Int, a: Int, b: Int):

    def abs: Constraint =
      if delta >= 0 then this else Constraint(-delta, b, a)

  /**
   * @see [[input24.txt]] For reverse engineering of the program.
   *
   *      This consist of 14 times the following block:
   *
   *      inp w
   *      mul x 0
   *      add x z
   *      mod x 26
   *      div z {a}
   *      add x {b}
   *      eql x w
   *      eql x 0
   *      mul y 0
   *      add y 25
   *      mul y x
   *      add y 1
   *      mul z y
   *      mul y 0
   *      add y w
   *      add y {c}
   *      mul y x
   *      add z y
   *
   *      Which decompiles into the following pseudocode:
   *
   *      w  = int(input())
   *      x  = int((z % 26) + b != w)
   *      z /= a
   *      z *= 25 * x + 1
   *      z += (w + c) * x
   *
   *      E.g. with my input for digit `0`, i.e. the first block:
   *
   *      inp w     -- D[0]
   *      mul x 0
   *      add x z
   *      mod x 26
   *      div z 1   -- a = 1
   *      add x 14  -- b = 14
   *      eql x w
   *      eql x 0
   *      mul y 0
   *      add y 25
   *      mul y x
   *      add y 1
   *      mul z y
   *      mul y 0
   *      add y w
   *      add y 12  -- c = 12
   *      mul y x
   *      add z y
   *
   *      Leads to:
   *
   *      w  = int(input())
   *      x  = int((z % 26) + 14 != w)
   *      z /= 1
   *      z *= 25 * x + 1
   *      z += (w + 12) * x
   *
   *      Another thing to note is that the a is 1 seven times and 26 the other seven
   *      times.  In a block where a is 1, b is always between 10 and 16. It follows
   *      that `z /= {a}` line is no-op and `(z % 26) + b != w` is always true. So
   *      the decompiled code becomes:
   *
   *      w = int(input())
   *      z *= 26
   *      z += w + c
   *
   *      So this block of code is "pushing" a digit of w + c in base 26. So to get 0
   *      at the end, we have to "pop" these digits back out using `z /= 26` and don't
   *      add any more back. Thus, in the lines with a = 26, x = int((z % 26) + b != w)
   *      must be 0, which means the last pushed digit w_old + c must be equal to w_now - b.
   *
   *      This makes the problem a constraint solving one. For my particular input:
   *
   *           {lhs:c}  {rhs:b}
   *      -----------------------
   *      D[0] + 12     -  6 == D[13]
   *      D[1] +  9     - 16 == D[12]
   *      D[4] +  0     -  5 == D[11]
   *      D[5] + 11     -  8 == D[10]
   *      D[8] +  3     -  1 == D[9]
   *      D[6] + 13     - 14 == D[7]
   *      D[2] +  8     -  8 == D[3]
   *
   */
  val constraints: Vector[Constraint] =
    Vector(
      Constraint( 6, 0, 13),
      Constraint(-7, 1, 12),
      Constraint(-5, 4, 11),
      Constraint( 3, 5, 10),
      Constraint( 2, 8,  9),
      Constraint(-1, 6,  7),
      Constraint( 0, 2,  3)
    )

  def solve(constraints: Vector[Constraint]): Vector[Range] =

    def go(n: Int): Vector[Range] =

      val digits: mutable.IndexedSeq[Range] =
        mutable.IndexedSeq.fill(n)(1 to 9)

      for
        Constraint(delta, i, j) <- constraints.map(_.abs)
      do
        digits(i) = digits(i).min to (9 - delta)
        digits(j) = (1 + delta) to digits(j).max

      digits.toVector

    go(14)

  lazy val answer1: String = solve(constraints).map(_.max).mkString
  lazy val answer2: String = solve(constraints).map(_.min).mkString
