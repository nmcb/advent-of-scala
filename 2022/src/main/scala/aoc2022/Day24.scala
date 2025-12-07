package aoc2022

import nmcb.*

import scala.annotation.tailrec
import scala.io.*

object Day24 extends AoC:

  case class Pos(x: Int, y: Int):
    def +(p: Pos): Pos = Pos(x + p.x, y + p.y)
    def n: Set[Pos] = Set(Pos(1,0),Pos(-1,0),Pos(0,1),Pos(0,-1)).map(this + _)

  object Pos:
    def start: Pos = Pos(-1, 0)
    def end(bounds: Bounds): Pos = Pos(bounds.max.x, bounds.max.y)

    given Ordering[Pos] with
      def compare(a: Pos, b: Pos): Int =
        Ordering[(Int,Int)].compare((a.y, a.x), (b.y, b.x))

  case class Bounds(max: Pos):
    val sizeX: Int = max.x + 1
    val sizeY: Int = max.y + 1
    val fieldSize: Int = sizeX * sizeY
    def isField(p: Pos): Boolean = p.x >= 0 && p.x <= max.x && p.y >= 0 && p.y <= max.y
    def transpose: Bounds = copy(max = Pos(max.y, max.x))

  enum Dir(val char: Char):
    case Up    extends Dir(char = Field.UpChar)
    case Down  extends Dir(char = Field.DownChar)
    case Left  extends Dir(char = Field.LeftChar)
    case Right extends Dir(char = Field.RightChar)

  type Field[A] = Vector[Vector[A]]

  object Field:

    val WallChar: Char  = '#'
    val OpenChar: Char  = '.'

    val UpChar:   Char  = '^'
    val DownChar: Char  = 'v'
    val LeftChar: Char  = '<'
    val RightChar: Char = '>'

    extension [A](field: Field[A]) def trimBorder: Field[A] =
      field.drop(1).dropRight(1).map(_.drop(1).dropRight(1))

    extension [A](b: Vector[Vector[A]]) def asString: String =
      b.map(_.mkString("")).mkString("\n","\n","\n")

  type Stream = LazyList[Char]

  object Stream:
    import Field.*
    def filter(dir: Dir, line: Vector[Char]): Vector[Char] =
      line.map(blizzard => if blizzard == dir.char then blizzard else OpenChar)

  case class Streams(u: Vector[Stream], d: Vector[Stream], l: Vector[Stream], r: Vector[Stream], bounds: Bounds):
    import Field.*
    import bounds.*
    def uAt(x: Int, y: Int): Char = u(x).drop(y).head
    def dAt(x: Int, y: Int): Char = d(x).drop(max.y - y).head
    def lAt(x: Int, y: Int): Char = l(y).drop(x).head
    def rAt(x: Int, y: Int): Char = r(y).drop(max.x - x).head

    def next: Streams =
      copy(u = u.map(_.tail), d = d.map(_.tail), l = l.map(_.tail), r = r.map(_.tail))

    def countBlizzards(streamField: Field[(Char,Char,Char,Char)])(x: Int, y: Int): Int =
      val (uc,dc,lc,rc) = streamField(y)(x)
      var c = 0
      if uc == UpChar    then c = c + 1
      if dc == DownChar  then c = c + 1
      if lc == LeftChar  then c = c + 1
      if rc == RightChar then c = c + 1
      c

    def nextBlizzardChar(streamField: Field[(Char,Char,Char,Char)])(x: Int, y: Int): Char =
      val count = countBlizzards(streamField)(x: Int, y: Int)
      val (uc,dc,lc,rc) = streamField(y)(x)
      count match
        case 0 => OpenChar
        case 1 if uc == UpChar    => UpChar
        case 1 if dc == DownChar  => DownChar
        case 1 if lc == LeftChar  => LeftChar
        case 1 if rc == RightChar => RightChar
        case 2 => '2'
        case 3 => '3'
        case 4 => '4'
        case _ => sys.error("more than 4 blizzards in one field pos")

    val futureStreamField: Field[(Char,Char,Char,Char)] =
      (for {
        y <- 0 until sizeY
        x <- 0 until sizeX
        uc = uAt(x, y)
        dc = dAt(x, y)
        lc = lAt(x, y)
        rc = rAt(x, y)
      } yield (uc,dc,lc,rc)).grouped(sizeX).map(_.toVector).toVector

    val futureField: Field[Char] =
      val buffer: Array[Array[Char]] = Array.fill(sizeY, sizeX)(Field.OpenChar)
      for (y <- 0 until sizeY ; x <- 0 until sizeX) {
        buffer(y)(x) = nextBlizzardChar(futureStreamField)(x, y)
      }
      buffer.map(_.toVector).toVector

  type Paths = Set[Pos]

  object Paths:
    def start: Paths = Set(Pos.start)

  case class World(
    target: Pos,
    currentField: Field[Char],
    nextField: Field[Char],
    streams: Streams,
    minutes: Int,
    bounds: Bounds,
    found: Paths
  ):
    import Field.*
    import bounds.*


    def  free(f: Field[Char])(p: Pos): Boolean =
      if bounds.isField(p) then
        f(p.y)(p.x) match
          case OpenChar => true
          case _        => false
      else
        false

    def paths: Paths =
      found.flatMap(p => {
          val ms   = p.n.filter(free(nextField))
          val test = free(nextField)(p) && p.n.exists(free(streams.futureField)) && bounds.isField(p)
          if test || p == Pos.end(bounds) + Pos(0,1) || p == Pos(0,-1) then
            ms + p
          else
            ms
      })

    def reachedGoal: Boolean =
      found.contains(target)

    def fewestMinutes: Int =
      minutes

    def next: World =
      World(target, nextField, streams.futureField, streams.next, minutes + 1, bounds, paths)

    override def toString: String =
      val hd = s"minutes=$minutes, target=$target\n"
      val nw = WallChar.toString + OpenChar.toString + List.fill(sizeX)(WallChar).mkString("")
      val ls = currentField.map(_.mkString(WallChar.toString, "", WallChar.toString)).mkString("\n","\n","\n")
      val sw = List.fill(sizeX)(WallChar).mkString("") + OpenChar.toString + WallChar.toString
      val ps = s"path=${found.toList.sorted.mkString("","","\n")}"
      "\n" + hd + nw + ls + sw + "\n" + ps + "\n---"

  object World:
    import Dir.*
    import Field.*

    def init: World =
      val initField: Field[Char] =
        Source
          .fromResource(s"$day.txt")
          .getLines
          .foldLeft(Vector.empty[Vector[Char]])(_ :+ _.toVector)
          .trimBorder

      assert(initField.forall(_.length == initField.map(_.length).max))
      val sizeX = initField.head.size
      val sizeY = initField.size
      val bounds: Bounds = Bounds(Pos(sizeX - 1 , sizeY - 1))

      def make(dir: Dir, f: Field[Char]): Vector[Stream] =
        def stream(line: Vector[Char]): LazyList[Char] =
          line.map(c => if c == dir.char then c else OpenChar).to(LazyList) #::: stream(line)
        dir match
          case Up    => f.transpose.foldLeft(Vector.empty)((v,l) => v :+ stream(l.tail :+ l.head))
          case Down  => f.transpose.foldLeft(Vector.empty)((v,l) => v :+ stream((l.last +: l.init).reverse))
          case Left  => f.foldLeft(Vector.empty)((v,l) => v :+ stream(l.tail :+ l.head))
          case Right => f.foldLeft(Vector.empty)((v,l) => v :+ stream((l.last +: l.init).reverse))

      def up: Vector[LazyList[Char]] = make(Up, initField)
      def down: Vector[LazyList[Char]] = make(Down, initField)
      def left: Vector[LazyList[Char]] = make(Left, initField)
      def right: Vector[LazyList[Char]] = make(Right, initField)
      def streams: Streams = Streams(up, down, left, right, bounds)

      World(Pos.end(bounds), initField, streams.futureField, streams.next, 0, bounds, Paths.start)

  @tailrec
  def solve1(world: World): Int =
    if world.reachedGoal then world.fewestMinutes else
      val n = world.next
      solve1(n)

  def solve2(world: World): Int =

    @tailrec
    def loop(w: World): (World, Int) =
      if w.reachedGoal then (w, w.fewestMinutes) else loop(w.next)

    val (w1, m1) = loop(world)

    val w2 = w1.copy(
      target       = Pos(0,0),
      currentField = w1.nextField,
      nextField    = w1.streams.futureField,
      streams      = w1.streams.next,
      minutes      = m1 + 1,
      found        = Set(Pos.end(w1.bounds) + Pos(0,1))
    )
    val (w3, m3) = loop(w2)

    val w4 = w3.copy(
      target       = Pos.end(w3.bounds),
      currentField = w3.nextField,
      nextField    = w3.streams.futureField,
      streams      = w3.streams.next,
      minutes      = m3 + 1,
      found        = Set(Pos(0,-1))
    )

    val (_, m5) = loop(w4)

    m5 + 1


  lazy val answer1: Int = solve1(World.init) + 1
  lazy val answer2: Int = solve2(World.init)
