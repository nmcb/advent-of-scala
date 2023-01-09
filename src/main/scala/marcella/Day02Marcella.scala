package marcella

import scala.io.Source

object Day02Marcella extends App :

  val input: List[(Hand, Hand)] =
    Source
      .fromResource("input02.txt")
      .getLines
      .map { case s"$them $me" => (shape(them), shape(me)) }
      .toList

  sealed trait Hand

  case object Rock extends Hand

  case object Scissors extends Hand

  case object Paper extends Hand

  def shape(input: String): Hand =
    input match {
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors
    }

  def game(me: Hand, them: Hand): Int =
    (me, them) match
      case (Rock, Paper) => 6 + 2
      case (Rock, Rock) => 3 + 1
      case (Rock, Scissors) => 0 + 3
      case (Paper, Scissors) => 6 + 3
      case (Paper, Paper) => 3 + 2
      case (Paper, Rock) => 0 + 1
      case (Scissors, Rock) => 6 + 1
      case (Scissors, Scissors) => 3 + 3
      case (Scissors, Paper) => 0 + 2

  val answer1: Int = input.map(game).sum
  println(s"Answer to Game 1 is: ${answer1}")

  def game2(me: Hand, them: Hand): Int =
    (me, them) match
      case (Rock, Paper) => 1 + 3
      case (Rock, Rock) => 3 + 0
      case (Rock, Scissors) => 2 + 6
      case (Paper, Scissors) => 3 + 6
      case (Paper, Paper) => 2 + 3
      case (Paper, Rock) => 1 + 0
      case (Scissors, Rock) => 2 + 0
      case (Scissors, Scissors) => 1 + 6
      case (Scissors, Paper) => 3 + 3

  val answer2: Int = input.map(game2).sum
  println(s"Answer to Game 2 is: ${answer2}")
