import scala.io.*

object Day18 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val input =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(_.filter(_ != ' '))
        .toList

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
    (braced(expr1) | value).chainl1(lassoc)

  def lassoc: P[BinOp] =
    infix("*")(lhs => rhs => Add(lhs, rhs)) | infix("+")(lhs => rhs => Mul(lhs, rhs))

  def value: P[Expr] =
    for v <- digit.oneOrMore yield Val(v.mkString.toLong)
  
  def parse1(line: String): Expr = 
    P.run(expr1)(line)


  val start1  = System.currentTimeMillis
  val answer1 = input.map(parse1).map(_.eval).sum
  println(s"Answer AOC 2020 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  // expr2   := lhs@term2 -> { '*' rhs@term2 }                       => Mul(lhs,rhs)
  // term2   := lhs@( '(' expr2 ')' | value ) -> { '+' rhs@expr2 }   => Add(lhs,rhs)
  // value   := digit -> { digit }                                   => Val(value)

  def expr2: P[Expr] =
    term2.chainl1(infix("*")(lhs => rhs => Mul(lhs,rhs)))

  def term2: P[Expr] =
    (braced(expr2)| value).chainl1(infix("+")(lhs => rhs => Add(lhs,rhs)))
  
  def parse2(line: String): Expr =
    run(expr2)(line)

  val start2  = System.currentTimeMillis
  val answer2 = input.map(parse2).map(_.eval).sum
  println(s"Answer AOC 2020 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
