package aoc2022

import nmcb.*

object Day13 extends AoC:

  enum E:
    case N(n: Int)
    case L(l: List[E])

  object E:
    def L(es: E*): L = L(es.toList)

    import P.*

    def n: P[E] = digits.map(N.apply)
    def l: P[E] = seq('[', ',', ']', e).map(L.apply)
    def e: P[E] = n | l
    def parse(s: String): E = e.run(s)

    given ordering: Ordering[E] with
      def compare(l: E, r: E): Int =
        (l,r) match
          case (ln: N, rl: L)             => compare(L(ln), rl)
          case (ll: L, rn: N)             => compare(ll, L(rn))
          case (N(ln), N(rn))             => ln.compare(rn)
          case (L(Nil), L(_ :: _))        => -1
          case (L(Nil), L(Nil))           =>  0
          case (L(_ :: _), L(Nil))        =>  1
          case (L(lh :: lt), L(rh :: rt)) =>
            compare(lh, rh) match
               case -1 => -1
               case  0 => compare(L(lt), L(rt))
               case  1 => 1


  /** Solutions */

  import E.{*, given}

  import math.Ordered.orderingToOrdered

  val expressions: List[E] =
    lines
      .filterNot(_.isBlank)
      .map(parse)
      .toList

  
  lazy val answer1: Int =
    expressions
      .grouped(2)
      .zipWithIndex
      .map((es,idx) => if es(0) <= es(1) then idx + 1 else 0)
      .sum

  lazy val answer2: Long =
    val divider1: E = L(L(N(2)))
    val divider2: E = L(L(N(6)))

    val ordered = (divider1 :: divider2 :: expressions).sorted
    val idx1 = ordered.indexWhere(_ == divider1) + 1
    val idx2 = ordered.indexWhere(_ == divider2) + 1
    idx1 * idx2


  /** Utilities */

  object P:
    case class P[A](parse: String => Option[(A,String)]):
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

      private def loop(s: String, acc: List[A] = List.empty): (List[A], String) =
        parse(s) match {
          case None         => (acc.reverse, s)
          case Some((a,ss)) => loop(ss, a :: acc)
        }

      def opt: P[Option[A]] =
        P(s => parse(s).map((a,ss) => (Some(a), ss)).orElse(Some(None,s)))

      def zeroOrMore: P[List[A]] =
        P(s => Some(loop(s)))

      def oneOrMore: P[List[A]] =
        P(s => parse(s).flatMap((a,ss) => Some(loop(ss, List(a)))))

      def |[A1 >: A](that: => P[A1]): P[A1] =
        P(s => parse(s) match {
          case None        => that.parse(s)
          case res@Some(_) => res
        })

      def &[B](that: => P[B]): P[(A,B)] =
        for { a <- this ; b <- that } yield (a, b)

      def ~[B](that: P[B]): P[B] =
        for { _ <- this ; b <- that } yield b

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
      satisfy(_.isDigit).oneOrMore.map(_.mkString("").toInt)

    def separated[A](sep: P[?], pa: P[A]): P[List[A]] =
      for { h <- pa.opt ; t <- (sep ~ pa).zeroOrMore } yield
        h.map(_ :: t).getOrElse(List.empty)

    def separated[A](sep: Char, pa: P[A]): P[List[A]] =
      separated(char(sep), pa)

    def seq[A](lhs: Char, sep: Char, rhs: Char, pa: P[A]): P[List[A]] =
      for { _ <- char(lhs) ; es <- separated(sep, pa) ; _ <- char(rhs) } yield es
