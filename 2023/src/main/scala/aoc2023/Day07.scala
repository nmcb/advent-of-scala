package aoc2023

import nmcb.*

import scala.annotation.tailrec
import scala.math.Ordered.orderingToOrdered

object Day07 extends AoC:

  case class Card(symbol: Char):
    def isJoker: Boolean = symbol == 'J'

  enum Strength:
    case High
    case Pair
    case TwoPair
    case ThreeOfAKind
    case FullHouse
    case FourOfAKind
    case FiveOfAKind

  import Strength.*

  given Ordering[Strength] with

    def compare(a: Strength, b: Strength): Int =
      (a, b) match
        case (FiveOfAKind, FiveOfAKind)   => 0
        case (FiveOfAKind, _)             => 1
        case (_, FiveOfAKind)             => -1
        case (FourOfAKind, FourOfAKind)   => 0
        case (FourOfAKind, _)             => 1
        case (_, FourOfAKind)             => -1
        case (FullHouse, FullHouse)       => 0
        case (FullHouse, _)               => 1
        case (_, FullHouse)               => -1
        case (ThreeOfAKind, ThreeOfAKind) => 0
        case (ThreeOfAKind, _)            => 1
        case (_, ThreeOfAKind)            => -1
        case (TwoPair, TwoPair)           => 0
        case (TwoPair, _)                 => 1
        case (_, TwoPair)                 => -1
        case (Pair, Pair)                 => 0
        case (Pair, _)                    => 1
        case (_, Pair)                    => -1
        case (High, High)                 => 0

  type Cards = List[Card]

  object Game:
    val set: Cards               = "AKQJT98765432".map(Card.apply).toList
    val sortedSet: Cards         = set.reverse
    val jokerSet: Cards          = "AKQT98765432J".map(Card.apply).toList
    val sortedJokerSet: Cards    = jokerSet.reverse
    val jokerReplacements: Cards = jokerSet.filterNot(_.isJoker)

  case class Hand(cards: Cards, bid: Int)(using Ordering[Card]):
    assert(cards.size == 5)

    import Game.*

    lazy val jokers: Int =
      cards.filter(_.isJoker).size

    lazy val nonJokers: Cards =
      cards.filterNot(_.isJoker)
    
    lazy val combinations: List[Cards] =

      @tailrec
      def replace(n: Int, acc: List[Cards] = List.fill(jokers)(List.empty[Card])): List[Cards] =
        if n <= 0 then
          acc
        else
          replace(n - 1, acc.flatMap(t => jokerReplacements.map(d => d :: t)))

      if jokers == 0 then
        List(cards)
      else
        replace(jokers).map(replacements => replacements ++ nonJokers)

    lazy val strength1: Strength =
      strengths(cards).max

    lazy val strength2: Strength =
      combinations.grouped(8).map(_.map(cs => strengths(cs).max).max).max

    private def strengths(hand: Cards): List[Strength] =
      def loop(todo: List[(Card, Int)], found: List[Strength] = List.empty): List[Strength] =
        todo match
          case Nil => found.sorted
          case (d, 5) :: rest => loop(rest,  FiveOfAKind :: found)
          case (d, 4) :: rest => loop(rest,  FourOfAKind :: found)
          case (d, 3) :: rest => loop(rest, ThreeOfAKind :: found)
          case (d, 2) :: rest =>
            found match
              case Nil               => loop(rest, Pair :: found)
              case Pair :: _         => loop(rest, TwoPair :: found.tail)
              case ThreeOfAKind :: _ => loop(rest, FullHouse :: found.tail)
              case _                 => sys.error(s"illegal state: found=$found, todo=$todo")
          case (d, 1) :: rest => loop(rest, High :: found)
          case _              => sys.error(s"illegal state: $todo")

      val countedCards = hand.groupMapReduce(identity)(_ => 1)(_ + _).toList.sortBy(_._2).reverse
      loop(countedCards).reverse

  def orderBy(sortedSet: Cards): Ordering[Card] =
    (a: Card, b: Card) => sortedSet.indexOf(a) compare sortedSet.indexOf(b)

  def orderBy(strength: Hand => Strength)(using Ordering[Card]): Ordering[Hand] =
    (a: Hand, b: Hand) =>
      val highs = strength(a) compare strength(b)
      if highs != 0 then highs else
        def highest(as: Cards, bs: Cards): Int =
          (as, bs) match
            case (ha :: ra, hb :: rb) if (ha compare hb) == 0 => highest(ra, rb)
            case (ha :: ra, hb :: rb)                         => ha compare hb
            case (Nil, Nil)                                   => 0
            case (_,_) => sys.error(s"could not compare: $as with $bs")
        highest(a.cards, b.cards)

  object Hand:
    def fromString(s: String)(using Ordering[Card]): Hand =
      s match
        case s"$cards $bid" =>
          Hand(cards.map(Card.apply).toList, bid.toInt)

  
  lazy val answer1: Int =
    given Ordering[Card] = orderBy(Game.sortedSet)
    lines
      .map(Hand.fromString)
      .sorted(using orderBy(_.strength1))
      .zipWithIndex
      .map((h,i) => h.bid * (i + 1))
      .sum

  lazy val answer2: Int =
    given Ordering[Card] = orderBy(Game.sortedJokerSet)
    lines
      .map(Hand.fromString)
      .sorted(using orderBy(_.strength2))
      .zipWithIndex
      .map((h,i) => h.bid * (i + 1))
      .sum
