package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day08 extends AoC:

  val puzzle =
    lines
      .map: line =>
        val input  = line.split("\\|")(0).trim.split(" ").toList
        val output = line.split("\\|")(1).trim.split(" ").toList
        (input, output)
      .toList

  
  val leds0 = Set(0,1,2,4,5,6)
  val leds1 = Set(2,5)
  val leds2 = Set(0,2,3,4,6)
  val leds3 = Set(0,2,3,5,6)
  val leds4 = Set(1,2,3,5)
  val leds5 = Set(0,1,3,5,6)
  val leds6 = Set(0,1,3,4,5,6)
  val leds7 = Set(0,2,5)
  val leds8 = Set(0,1,2,3,4,5,6)
  val leds9 = Set(0,1,2,3,5,6)

  type Wiring = String

  def decode(wiring: Wiring)(digit: String): Option[Int] =
    val leds = digit.map(code => wiring.indexOf(code)).toSet
    if      (leds == leds0) Some(0)
    else if (leds == leds1) Some(1)
    else if (leds == leds2) Some(2)
    else if (leds == leds3) Some(3)
    else if (leds == leds4) Some(4)
    else if (leds == leds5) Some(5)
    else if (leds == leds6) Some(6)
    else if (leds == leds7) Some(7)
    else if (leds == leds8) Some(8)
    else if (leds == leds9) Some(9)
    else None

  def valid(wiring: Wiring)(input: List[String]): Boolean =

    @tailrec
    def loop(todo: List[String], digitsFound: Set[Int] = Set.empty): Boolean =

      val allDigits: Set[Int] = Set(0,1,2,3,4,5,6,7,8,9)

      def inDigitsFound(digit: Int): Boolean =
        digitsFound.contains(digit)

      def notInDigitsLeftToValidate(digit: Int): Boolean =
        !(allDigits -- digitsFound).contains(digit)

      todo match
        case Nil => digitsFound == allDigits
        case encoding :: encodings => decode(wiring)(encoding) match
          case None => false
          case Some(digit) if inDigitsFound(digit) => false
          case Some(digit) if notInDigitsLeftToValidate(digit) => false
          case Some(digit) => loop(encodings, digitsFound + digit)

    loop(input)


  lazy val answer1: Int =
    puzzle
      .map: (_,output) =>
        output.count: digit =>
          val is1 = digit.length == 2
          val is7 = digit.length == 3
          val is4 = digit.length == 4
          val is8 = digit.length == 7
          is1 || is4 || is7 || is8
      .sum

  lazy val answer2: Int =
    @tailrec
    def search(wirings: List[String])(input: List[String]): String =
      val wiring = wirings.head
      if valid(wiring)(input) then
        wiring
      else 
        search(wirings.tail)(input)

    val init: List[Wiring] =
      "abcdefg".permutations.toList

    val numbers: List[Int] =
      puzzle.zipWithIndex.map:
        case ((input,output),idx) =>
          val wiring: Wiring =
            search(init)(input)
          val number: Int =
            output
              .foldLeft(""): (num,digit) =>
                num + decode(wiring)(digit).get
              .toInt
          number

    numbers.sum
