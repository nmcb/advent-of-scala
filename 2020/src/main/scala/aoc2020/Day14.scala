package aoc2020

import nmcb.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day14 extends AoC:

  case class UInt36(underlying: Long):
    assert(underlying.toBinaryString.length <= 36)

    def this(string: String, radix: Int) = 
      this(BigInt(string, radix).longValue)

    def toBinaryString: String = 
      List.fill(36 - underlying.toBinaryString.length)('0').mkString + underlying.toBinaryString

    override def toString: String =
      underlying.toString

  val MaskExpr: Regex   = "mask = (.+)".r
  val UpdateExpr: Regex = "mem\\[(\\d+)] = (\\d+)".r

  sealed trait Inst

  case class Mask(underlying: String) extends Inst:
    assert(underlying.count(c => c == 'X' || c == '1' || c == '0') == 36)

    def mask(value: UInt36): UInt36 =
      new UInt36(
        string = underlying
                   .zip(value.toBinaryString)
                   .foldRight(""):
                     case (('X',b), lsb) => "" +  b  + lsb
                     case (('1',_), lsb) => "" + '1' + lsb
                     case (('0',_), lsb) => "" + '0' + lsb
                     case err => sys.error(s"boom: $err"),
        radix = 2
      )

    def float(address: UInt36): List[UInt36] =
      underlying
        .zip(address.toBinaryString)
        .foldRight(""):
          case (('X',_),lsb) => "" + 'X' + lsb
          case (('1',_),lsb) => "" + '1' + lsb
          case (('0',b),lsb) => "" +  b  + lsb
          case err => sys.error(s"boom: $err")
        .foldRight(List.empty[String]):
          case ('X',acc) => if acc.nonEmpty then acc.flatMap(n => List("0" + n, "1" + n)) else List("0", "1")
          case ('1',acc) => if acc.nonEmpty then acc.map(n => "1" + n) else List("1")
          case ('0',acc) => if acc.nonEmpty then acc.map(n => "0" + n) else List("0")
          case err => sys.error(s"boom: $err")
        .map(string => new UInt36(string, 2))

    override def toString: String =
      underlying

  object Mask:
    def fill(c: Char): Mask = Mask(List.fill(36)(c).mkString)

  case class Update(addres: UInt36, value: UInt36) extends Inst

  type Memory  = Map[UInt36,UInt36]
  type Program = List[Inst]

  val program: Program = lines.toList.map:
    case UpdateExpr(address,value) => Update(new UInt36(address, 10), new UInt36(value, 10))
    case MaskExpr(mask)            => Mask(mask)

  def run(program: Program)(codec: UInt36 => UInt36 => Mask => Memory => Memory): Long =

    @tailrec
    def loop(program: Program, memory: Memory = Map.empty, mask: Mask = Mask.fill('X')) : Memory =
      program match
        case Nil          => memory
        case inst :: rest => inst match
            case Update(address,value) => loop(rest, codec(address)(value)(mask)(memory), mask)
            case mask: Mask            => loop(rest, memory , mask)

    loop(program).values.map(_.underlying).sum


  lazy val answer1: Long = run(program)(address => value => mask => memory => memory ++ Map(address -> mask.mask(value)))
  lazy val answer2: Long = run(program)(address => value => mask => memory => memory ++ mask.float(address).map(a => a -> value))
