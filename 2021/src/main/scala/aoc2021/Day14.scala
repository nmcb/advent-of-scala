package aoc2021

import nmcb.*
import nmcb.predef.*
object Day14 extends AoC:

  type Rules  = Map[(Char,Char),Char]
  type Pairs  = Map[(Char,Char),Long]
  type Counts = Map[Char,Long]

  val template: String =
    lines.head

  val rules: Rules =
    lines
      .collect:
        case s"$pair -> $insert" => (pair.charAt(0), pair.charAt(1)) -> insert.charAt(0)
      .toMap

  def fst[A](pair: (A,?)): A =
    pair._1

  def snd[B](pair: (?,B)): B =
    pair._2

  /** maintain a count for sequences of pairs as well as individual molecules */
  case class Polymer(rules: Rules, pairs: Pairs, counts: Counts):

    def step: Polymer =

      val nextPairs: Pairs =
        pairs
          .toVector
          .flatMap: (pair,count) =>
            val char = rules(pair)
            Vector((fst(pair),char) -> count, (char, snd(pair)) -> count)
          .groupMapReduce(fst)(snd)(_ + _)

      val nextCounts: Counts =
        pairs
          .foldLeft(counts): (result,count) =>
            val char = rules(fst(count))
            result.updated(char, result(char) + snd(count))
          .groupMapReduce(fst)(snd)(_ + _)

      copy(pairs = nextPairs, counts = nextCounts)

  object Polymer:

    def make(rules: Rules, template: String): Polymer =
      val pairs  = template.zip(template.tail).groupMapReduce(identity)(_ => 1L)(_+_)
      val counts = template.groupMapReduce(identity)(_ => 1L)(_ + _)
      Polymer(rules, pairs, counts)

  def solve(polymer: Polymer, iterations: Int): Long =
    val counts = Iterator.iterate(polymer)(_.step).nth(iterations).counts
    counts.values.max - counts.values.min

  val polymer: Polymer = Polymer.make(rules, template)

  lazy val answer1: Long = solve(polymer, 10)
  lazy val answer2: Long = solve(polymer, 40)
