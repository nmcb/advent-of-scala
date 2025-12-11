package aoc2020

import nmcb.*

object Day16 extends AoC:

  case class Rule(pattern: String):

    private val (a, b, c, d) = pattern match
      case s"${name}: $a-$b or $c-$d" => (a.toInt, b.toInt, c.toInt, d.toInt)

    def check(i: Int): Boolean = (a <= i && i <= b) || (c <= i && i <= d)

  val (rules: Set[Rule], mine: Seq[Int], nearby: Seq[Seq[Int]]) =
    val (rules, rest)  = lines.splitAt(lines.indexOf(""))
    val (your, nearby) = (rest(2), rest.drop(5))
    (rules.map(Rule.apply).toSet, your.split(",").map(_.toInt).toSeq, nearby.map(_.split(",").map(_.toInt).toSeq))

  def solve1(rules: Set[Rule], mine: Seq[Int], nearby: Seq[Seq[Int]]): Long =
    nearby.flatMap(ticket => ticket.filterNot(field => rules.exists(rule => rule.check(field)))).sum

  def solve2(rules: Set[Rule], mine: Seq[Int], nearby: Seq[Seq[Int]]): Long =
    val valid = nearby.filter(ticket => ticket.forall(field => rules.exists(rule => rule.check(field))))
    val sets  = valid.transpose.map(fields => rules.filter(rule => fields.forall(field => rule.check(field))))

    def refine(sets: Seq[Set[Rule]]): Seq[Set[Rule]] =
      val definite = sets.filter(_.size == 1).flatten
      sets.map(set => if set.size == 1 then set else set -- definite)

    val found = Iterator.iterate(sets)(refine).dropWhile(_.exists(_.size > 1)).next.map(_.head)
    val departures = found.zip(mine).filter((rule,field) => rule.pattern.startsWith("departure"))
    departures.map((_,field) => field.toLong).product


  lazy val answer1: Long = solve1(rules, mine, nearby)
  lazy val answer2: Long = solve2(rules, mine, nearby)
