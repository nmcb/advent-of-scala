package aoc2023

import nmcb.*

import scala.annotation.*

object Day04 extends AoC:

  case class Card(id: Int, winning: Vector[Int], mine: Vector[Int]):

    def scratch(score: Int => Int): Int =
      mine.foldLeft(0)((s,n) => if winning.contains(n) then if s == 0 then 1 else score(s) else s)

    def points: Int =
      scratch(_ * 2)

    def matching: Int =
      scratch(_ + 1)

  object Card:

    def fromString(s: String): Card =

      def ints(s: String): Vector[Int] =
        s.trim.split("\\s+").map(_.toInt).toVector

      s match
        case s"Card $id: $winning | $mine" => Card(id.trim.toInt, ints(winning), ints(mine))
        case _ => sys.error(s"unmatched: $s")

  val cards: Map[Int,Card] = lines.map(Card.fromString).map(c => c.id -> c).toMap

  def solve2(cards: Map[Int,Card]): Int =
    val maxId: Int =
      cards.keys.max

    def update(from: Int)(counts: Map[Int,Int], id: Int): Map[Int,Int] =
      counts.updatedWith(id)(_.map(_ + counts(from)))

    @tailrec
    def scratch(id: Int = 1, counts: Map[Int,Int] = cards.map((i,c) => i -> 1)): Int =
      if id > maxId then
        counts.view.values.sum
      else
        val next  = (id + 1 to id + cards(id).matching).foldLeft(counts)(update(id))
        scratch(id + 1, next)
    scratch()


  lazy val answer1: Int = cards.view.values.map(_.points).sum
  lazy val answer2: Int = solve2(cards)
