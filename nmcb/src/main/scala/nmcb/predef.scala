package nmcb

import scala.collection.*

object predef:

  extension [A](i: Iterator[A])

    def nth(n: Int): A =
      i.drop(n).next()

    def findFirst(f: A => Boolean): A =
      i.dropWhile(a => !f(a)).next()

    def findFirstNot(f: A => Boolean): A =
      i.dropWhile(a => f(a)).next()

    def findMap[B](f: A => Option[B]): B =
      i.flatMap(f).next()

    def drain: A =
      var a: A = i.next
      while i.hasNext do a = i.next
      a

  extension [A](i: Iterable[A])

    def pairs[B](order: ((A,A)) => B = identity)(using Ordering[B]): Iterator[(A,A)] =
      i.tails
        .toVector
        .tail
        .flatMap(i.zip)
        .sortBy(order)
        .iterator

  extension (s: String)
    def leftPadTo(length: Int, char: Char): String =
      List.fill(length - s.length)(char).mkString + s

  extension [A](t: (Pos,A))
    def pos: Pos   = t._1
    def element: A = t._2

  extension [A](s: Seq[A])
    def toTuple: (A,A) = (s(0), s(1))

  def memo[K,V](initial: (K,V)*): mutable.Map[K,V] =
    mutable.Map.empty[K,V] ++ initial

  extension [K,V](cache: mutable.Map[K,V])
    def memoize(k: K)(v: => V): V =
      cache.getOrElseUpdate(k, v)

  extension [A,B](p: (A,B))
    def left: A  = p._1
    def right: B = p._2
