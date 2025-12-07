import scala.annotation.*
import scala.io.*

object Day12 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  /** Utilities */

  case class P[+A](parse: String => Option[(A,String)]):
    def run(s: String): A =
      parse(s) match
        case Some(a,   "") => a
        case Some(_, rest) => sys.error(s"unconsumed at ${rest.take(10)}")
        case None          => sys.error(s"failed to parse")

    def map[B](f: A => B): P[B] =
      P(s => parse(s) match
        case Some(a, r) => Some(f(a), r)
        case None       => None
      )

    def flatMap[B](f: A => P[B]): P[B] =
      P(s => parse(s) match
        case Some(a, r) => f(a).parse(r)
        case None       => None
      )

    @tailrec
    private def loop[B >: A](s: String, acc: List[B] = List.empty): (List[B], String) =
      parse(s) match {
        case None         => (acc.reverse, s)
        case Some((a,ss)) => loop(ss, a :: acc)
      }

    def zeroOrMore: P[List[A]] =
      P(s => Some(loop(s)))

    def oneOrMore: P[List[A]] =
      P(s => parse(s).flatMap((a,ss) => Some(loop(ss, List(a)))))

    def |[A1 >: A](that: => P[A1]): P[A1] =
      P(s => parse(s) match {
        case None        => that.parse(s)
        case res@Some(_) => res
      })

    def ~[B](that: => P[B]): P[B] =
      for { _ <- this ; b <- that } yield b

  object P:
    def unit[A](a: A): P[A] =
      P(s => Some(a, s))

    def fail[A]: P[A] =
      P(_ => None)

    def take: P[Char] =
      P(s => if s.nonEmpty then Some(s.head, s.tail) else None)

    def satisfy(p: Char => Boolean): P[Char] =
      take.flatMap(c => if p(c) then unit(c) else fail)

    def char(c: Char): P[Char] =
      satisfy(_ == c)

    def digit: P[Char] =
      satisfy(_.isDigit)

    def digits: P[Int] =
      digit.oneOrMore.map(_.mkString("").toInt)

    def separated[A](sep: Char, pa: => P[A]): P[List[A]] =
      for { h <- pa ; t <- (char(sep) ~ pa).zeroOrMore } yield h :: t


  /** Modeling */

  object Json:
    import P.*

    val string: P[String] =
      for { _ <- char('"') ; s <- satisfy(_.isLetter).zeroOrMore ; _ <- char('"') } yield s.mkString

    val number: P[Int] =
      (for { _ <- char('-') ; i <- digits } yield -i) | digits

    val member: P[(String,Json)] =
      for { k <- string ; _ <- char(':') ; v <- json } yield (k,v)

    val obj: P[Json] =
      for { _ <- char('{') ; ms <- separated(',', member) ; _ <- char('}') } yield Obj(ms.toMap)

    val arr: P[Json] =
      for { _ <- char('[') ; es <- separated(',', json) ; _ <- char(']') } yield Arr(es)

    val num: P[Json] =
      number.map(i => Num(i))

    val str: P[Json] =
      string.map(s => Str(s))

    val json: P[Json] =
      arr | obj | num | str

    def parse(s: String): Json =
      json.run(s)

  sealed trait Json
  case class Str(underlying: String)           extends Json
  case class Num(underlying: Int)              extends Json
  case class Arr(underlying: List[Json])       extends Json
  case class Obj(underlying: Map[String,Json]) extends Json

  def solve(json: Json, objValueFilter: Json => Boolean = _ => true): Int =
    def loop(acc: Int, json: Json): Int =
      json match
        case Str(_)  => acc
        case Num(n)  => acc + n
        case Arr(js) => js.foldLeft(acc)(loop)
        case Obj(ms) => if ms.values.forall(objValueFilter) then ms.values.foldLeft(acc)(loop) else acc
    loop(0, json)

  /** Part 1 */

  lazy val input: Json = Json.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = solve(input)
  println(s"Answer AOC 2015 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = solve(input, _ != Str("red"))
  println(s"Answer AOC 2015 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
