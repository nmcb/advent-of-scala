package aoc2022

import nmcb.*

object Day09 extends AoC:

  enum Dir:
    case U
    case D
    case R
    case L

  import Dir.*

  case class Pos(x: Int, y: Int):

    infix def move(d: Dir): Pos =
      d match
        case U => copy(y = y + 1)
        case D => copy(y = y - 1)
        case L => copy(x = x - 1)
        case R => copy(x = x + 1)

    private def alignment(p: Pos): List[Dir] =
      val hor = x.compare(p.x) match
        case -1 if p.x - x >= 2 => // x < p.x
          y.compare(p.y) match
            case -1 => List(R,U)   // y < p.y
            case  0 => List(R)     // y = p.y
            case  1 => List(R,D)   // y > p.y

        case 1 if x - p.x >= 2 =>  // x > p.x
          y.compare(p.y) match
            case -1 => List(L,U)   // y < p.y
            case  0 => List(L)     // y = p.y
            case  1 => List(L,D)   // y > p.y

        case _ => List()

      val ver = y.compare(p.y) match
        case -1 if p.y - y >= 2 => // y < p.y
          x.compare(p.x) match
            case -1 => List(U,R)   // x < p.x
            case  0 => List(U)     // x = p.x
            case  1 => List(U,L)   // x > p.x

        case 1 if y - p.y >= 2 =>  // y > p.y
          x.compare(p.x) match
            case -1 => List(D,R)   // x < p.x
            case  0 => List(D)     // x = p.x
            case  1 => List(D,L)   // x > p.x

        case _ => List()

      List(hor,ver).flatten.distinct

    def follow(h: Pos): Pos =
      alignment(h).foldLeft(this)(_ move _)

  object Pos:
    def of(x: Int, y: Int): Pos = Pos(x,y)

  case class Cmd(dir: Dir, steps: Int)

  lazy val commands: Vector[Cmd] = lines.map:
      case s"U $s" => Cmd(U, s.toInt)
      case s"D $s" => Cmd(D, s.toInt)
      case s"L $s" => Cmd(L, s.toInt)
      case s"R $s" => Cmd(R, s.toInt)

  case class Bac(strep: List[Pos]):

    private def step(d: Dir, s: List[Pos]): List[Pos] =
      val nh = s.head.move(d)
      s.tail.foldLeft(List(nh))((a,t) => a :+ t.follow(a.last))

    infix def move(cmd: Cmd): List[Bac] =
      List
        .fill(cmd.steps)(cmd.dir)
        .foldLeft(List(this))((rs,d) => rs :+ Bac(step(d, rs.last.strep)))

  object Bac:

    def of(size: Int): Bac =
      Bac(List.fill(size)(Pos.of(0,0)))

    def solve(commands: Vector[Cmd], size: Int): Int =
      commands
        .foldLeft(List(Bac.of(size)))((p,c) => p ++ p.last.move(c))
        .map(_.strep.last)
        .distinct
        .size


  lazy val answer1: Int = Bac.solve(commands, 2)
  lazy val answer2: Int = Bac.solve(commands, 10)
