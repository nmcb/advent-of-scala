package aoc2020

import nmcb.*

object Day18 extends AoC:

  val puzzle: List[String] = lines.map(_.filter(_ != ' ')).toList

  enum Expr:
    case Add(l: Expr, r: Expr)
    case Mul(l: Expr, r: Expr)
    case Val(v: Long)

    def eval: Long =
      this match
        case Add(lhs, rhs) => lhs.eval + rhs.eval
        case Mul(lhs, rhs) => lhs.eval * rhs.eval
        case Val(v) => v

  import Expr.*
  import P.*

  def braced(expr: => P[Expr]): P[Expr] =
    for
      _     <- keyword("(")
      value <- expr
      _     <- keyword(")")
    yield
      value

  type BinOp = Expr => Expr => Expr

  def infix(op: String)(f: BinOp): P[BinOp] = 
    keyword(op) ~ unit(f)


  // expr1  := lhs@( '(' expr1 ')' | value ) -> { lassoc }
  // lassoc := '*' rhs@expr1 | '+' rhs@expr1                 => Mul(lhs,rhs) | Add(lhs,rhs)
  // value  := digit -> { digit }                            => Val(value)

  def expr1: P[Expr] =
    (braced(expr1) | value).chainLeftAssoc(lassoc)

  def lassoc: P[BinOp] =
    infix("*")(lhs => rhs => Add(lhs, rhs)) | infix("+")(lhs => rhs => Mul(lhs, rhs))

  def value: P[Expr] =
    for v <- digit.oneOrMore yield Val(v.mkString.toLong)
  
  def parse1(line: String): Expr = 
    P.run(expr1)(line)

  
  // expr2   := lhs@term2 -> { '*' rhs@term2 }                       => Mul(lhs,rhs)
  // term2   := lhs@( '(' expr2 ')' | value ) -> { '+' rhs@expr2 }   => Add(lhs,rhs)
  // value   := digit -> { digit }                                   => Val(value)

  def expr2: P[Expr] =
    term2.chainLeftAssoc(infix("*")(lhs => rhs => Mul(lhs,rhs)))

  def term2: P[Expr] =
    (braced(expr2)| value).chainLeftAssoc(infix("+")(lhs => rhs => Add(lhs,rhs)))
  
  def parse2(line: String): Expr =
    run(expr2)(line)


  lazy val answer1: Long = puzzle.map(parse1).map(_.eval).sum
  lazy val answer2: Long = puzzle.map(parse2).map(_.eval).sum
