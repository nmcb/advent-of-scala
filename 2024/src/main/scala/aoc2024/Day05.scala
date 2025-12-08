package aoc2024

import nmcb.AoC

object Day05 extends AoC:

  case class Rule(before: Int, after: Int):

    def applicable(x: Int, y: Int): Boolean =
      (before == x && after == y) || (before == y && after == x)

  case class Update(order: List[Int]):

    private val unvalidated: Map[Int, List[Int]] =
      def loop(todo: List[Int], result: Map[Int, List[Int]] = Map.empty[Int, List[Int]]): Map[Int, List[Int]] =
        todo match
          case Nil       => result
          case p :: rest => loop(rest, result + (p -> rest))
      loop(order)

    def validBy(rules: List[Rule]): Boolean =
      rules.forall: rule =>
        unvalidated.forall: (first, rest) =>
          rest.forall: page =>
            !(rule.before == page && rule.after == first)

    def middle: Int =
      order(order.length / 2)

    def sorted(using ordering: Ordering[Int]): Update =
      Update(order.sorted)

  private val (rules: List[Rule], updates: List[Update]) =
    val Array(rules, updates)  = input.split("\n\n")
    ( rules.split("\n")
        .map:
          case s"$b|$a" => Rule(b.toInt, a.toInt)
        .toList
    , updates.split("\n")
        .map: line =>
          Update(line.split(',').map(_.toInt).toList)
        .toList
    )

  given ruleBasedOrdering: Ordering[Int] =
    (x: Int, y: Int) =>
      rules.find(_.applicable(x, y)) match
        case None    => 0
        case Some(r) => if r.before == x && r.after == y then 1 else -1

  lazy val answer1: Int = updates.filter(_.validBy(rules)).map(_.middle).sum
  lazy val answer2: Int = updates.filterNot(_.validBy(rules)).map(_.sorted).map(_.middle).sum
