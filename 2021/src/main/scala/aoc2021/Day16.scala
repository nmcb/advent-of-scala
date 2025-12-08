package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day16 extends AoC:

  val puzzle: String =
    "005173980232D7F50C740109F3B9F3F0005425D36565F202012CAC0170004262EC658B0200FC3A8AB0EA5FF3312015070037100042"
    + "62243F8F600086C378B7152529CB4981400B202D04C00C0028048095070038C00B50028C00C50030805D3700240049210021C008"
    + "10038400A400688C00C3003E605A4A19A62D3E741480261B00464C9E6A5DF3A455999C2430E0054FCBE7260084F4B37B2D600343"
    + "25DE114B66A3A4012E4FFC62801069839983820061A60EE7526781E513C8050D00042E34C24898000844608F70E840198DD15226"
    + "2801D382460164D9BCE14CC20C179F17200812785261CE484E5D85801A59FDA64976DB504008665EB65E97C52DCAA82803B12646"
    + "04D342040109E802B09E13CBC22B040154CBE53F8015796D8A4B6C50C01787B800974B413A5990400B8CA6008CE22D003992F9A2"
    + "BCD421F2C9CA889802506B40159FEE0065C8A6FCF66004C695008E6F7D1693BDAEAD2993A9FEE790B62872001F54A0AC7F9B2C95"
    + "9535EFD4426E98CC864801029F0D935B3005E64CA8012F9AD9ACB84CC67BDBF7DF4A70086739D648BF396BFF603377389587C622"
    + "11006470B68021895FCFBC249BCDF2C8200C1803D1F21DC273007E3A4148CA4008746F8630D840219B9B7C9DFFD2C9A8478CD3F9"
    + "A4974401A99D65BA0BC716007FA7BFE8B6C933C8BD4A139005B1E00AC9760A73BA229A87520C017E007C679824EDC95B732C9FB0"
    + "4B007873BCCC94E789A18C8E399841627F6CF3C50A0174A6676199ABDA5F4F92E752E63C911ACC01793A6FB2B84D0020526FD26F"
    + "6402334F935802200087C3D8DD0E0401A8CF0A23A100A0B294CCF671E00A0002110823D4231007A0D4198EC40181E802924D3272"
    + "BE70BD3D4C8A100A613B6AFB7481668024200D4188C108C401D89716A080"

  enum Bit:
    case O, I

    def asString: String =
      this match
        case O => "0"
        case I => "1"

  import Bit.*

  type Version = Int       // 3 lsb
  type Id      = Int       // 3 lsb
  type Word    = Vector[Bit] // grouped 4 lsb

  case class Group(last: Boolean, word: Word)
  object Group:

    def apply(bits: Vector[Bit]): Group =
      Group(isLast(bits), bits.drop(1))

    def isLast(bits: Vector[Bit]): Boolean =
      bits.head == O

  enum Packet(val version: Version, val id: Id, val subs: Vector[Packet], val value: Long):
    case Lit(a: Version, b: Id, c: Long)         extends Packet(a, b, Vector.empty, c)
    case Sum(a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, c.map(_.value).sum)
    case Prd(a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, c.map(_.value).product)
    case Min(a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, c.map(_.value).min)
    case Max(a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, c.map(_.value).max)
    case Gt (a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, if c(0).value >  c(1).value then 1 else 0)
    case Lt (a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, if c(0).value <  c(1).value then 1 else 0)
    case Eq (a: Version, b: Id, c: Vector[Packet]) extends Packet(a, b, c, if c(0).value == c(1).value then 1 else 0)

    def versionSum: Int =
      version + subs.map(_.versionSum).sum

  object Packet:

    def apply(input: String): Packet =

      @tailrec
      def unwrap(bits: String, idx: Int = 0, acc: Vector[Bit] = Vector.empty): Vector[Bit] =
        if idx >= 8 then
          acc
        else
          unwrap(bits.tail, idx + 1, acc :+ (if bits.head == '0' then O else I))

      val (packet, _) =
        parse(
          input
            .grouped(2)
            .map(hex => String.format("%8s", Integer.parseInt(hex, 16).toBinaryString).replace(' ', '0'))
            .flatMap(bytes => unwrap(bytes))
            .toVector
        )

      packet


    def parse(bits: Vector[Bit]): (Packet, Vector[Bit]) =

      header(bits) match
        case (version, 4, rest)  =>
          val (gs, tail) = groups(rest)
          (Lit(version, 4, long(gs.foldLeft(Vector.empty)((a,g) => a ++ g.word))), tail)

        case (version, id, rest) if rest.head == O =>
          val length  = rest.slice(1, 16)
          val chunked = rest.slice(16, int(length) + 16)
          val (ps, _) = subs(chunked, None)
          (opp(version, id, ps), rest.drop(16 + int(length)))

        case (version, id, rest) if rest.head == I =>
          val length = rest.slice(1, 12)
          val (ps, tail) = subs(rest.drop(12), Some(int(length)))
          (opp(version, id, ps), tail)
          
        case (_, _, _) =>
          sys.error("boom!")

    def groups(rest: Vector[Bit]): (Vector[Group], Vector[Bit]) =

      @tailrec
      def go(groups: Vector[Vector[Bit]], acc: Vector[Group] = Vector.empty): (Vector[Group], Vector[Bit]) =
        groups match
          case bits +: rest if Group.isLast(bits) => (acc :+ Group(bits), rest.flatten)
          case bits +: rest                       => go(rest, acc :+ Group(bits))
          case _                                  => sys.error("no last group")

      go(rest.grouped(5).toVector)

    @tailrec
    def subs(bs: Vector[Bit], stop: Option[Int], acc: Vector[Packet] = Vector.empty): (Vector[Packet], Vector[Bit]) =
      if bs.isEmpty || stop.contains(acc.size) then
        (acc, bs)
      else
        val (p, r) = parse(bs)
        subs(r, stop, acc :+ p)

    def opp(version: Version, id: Id, subs: Vector[Packet]): Packet =
      id match
        case 0 => Sum(version, id, subs)
        case 1 => Prd(version, id, subs)
        case 2 => Min(version, id, subs)
        case 3 => Max(version, id, subs)
        case 5 => Gt (version, id, subs)
        case 6 => Lt (version, id, subs)
        case 7 => Eq (version, id, subs)

    def long(bits: Vector[Bit]): Long =
      java.lang.Long.parseLong(bits.map(_.asString).mkString(""), 2)

    def int(bits: Vector[Bit]): Int =
      long(bits).toInt

    def header(bits: Vector[Bit]): (Version, Id, Vector[Bit]) =
      (int(bits.take(3)), int(bits.slice(3, 6)), bits.drop(6))

  lazy val answer1: Int  = Packet(puzzle).versionSum
  lazy val answer2: Long = Packet(puzzle).value
