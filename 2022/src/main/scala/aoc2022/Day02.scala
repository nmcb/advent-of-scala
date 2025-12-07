package aoc2022

import nmcb.*

object Day02 extends AoC:

  val codes: Vector[(Char,Char)] = lines.map:
    case s"$p1 $p2" => (p1.head,p2.head)

  def score1(p1: Char, p2: Char): Int =
    (p1,p2) match
      case ('A','X') => 1 + 3
      case ('A','Y') => 2 + 6
      case ('A','Z') => 3 + 0
      case ('B','X') => 1 + 0
      case ('B','Y') => 2 + 3
      case ('B','Z') => 3 + 6
      case ('C','X') => 1 + 6
      case ('C','Y') => 2 + 0
      case ('C','Z') => 3 + 3
      case _ => sys.error(s"illegal chars: ($p1,$p2)")

  def score2(p1: Char, p2: Char): Int =
    (p1,p2) match
      case ('A','X') => 3 + 0
      case ('A','Y') => 1 + 3
      case ('A','Z') => 2 + 6
      case ('B','X') => 1 + 0
      case ('B','Y') => 2 + 3
      case ('B','Z') => 3 + 6
      case ('C','X') => 2 + 0
      case ('C','Y') => 3 + 3
      case ('C','Z') => 1 + 6
      case _ => sys.error(s"illegal chars: ($p1,$p2)")

  
  lazy val answer1: Int = codes.map(score1).sum
  lazy val answer2: Int = codes.map(score2).sum
