import scala.io.*

object Day03 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Pos(x: Int, y: Int):
    def +(p: Pos): Pos =
      Pos(x + p.x, y + p.y)

  case class Page(chars: Vector[Vector[Char]]):

    def size: Pos =
      Pos(chars.map(_.size).max, chars.size)

    val positions: Map[Pos,Char] =
      chars.zipWithIndex.flatMap((l,y) => l.zipWithIndex.map((c,x) => Pos(x, y) -> c)).toMap

    val digits: Map[Pos,Char] =
      positions.filter((_,c) => c.isDigit)

    val symbols: Map[Pos,Char] =
      positions.filterNot((_,c) => c == '.' || c.isDigit)

    val gears: Map[Pos, Char] =
      symbols.filter((_, c) => c == '*')

    def adjacents(pos: Pos): Set[Pos] =
      Set(Pos(1,-1), Pos(1,0), Pos(1,1), Pos(0,-1), Pos(0,0), Pos(0,1), Pos(-1,-1), Pos(-1,0), Pos(-1,1))
        .map(d => pos + d)
        .filter(p => p.x >= 0 && p.x < size.x && p.y >= 0 && p.y < size.y)

    def hasAdjacentSymbol(pos: Pos): Boolean =
      adjacents(pos).exists(symbols.contains)

    def isDigit(pos: Pos): Boolean =
      digits.contains(pos)

    def char(p: Pos): Char =
      chars(p.y)(p.x)

    @scala.annotation.tailrec
    final def numbers(p: Pos = Pos(0,0), c: String = "", l: Set[Pos] = Set.empty, found: Vector[(Long,Set[Pos])] = Vector.empty): Vector[(Long,Set[Pos])] =
      if p.y >= size.y then
        found
      else
        if p.x >= size.x then
          numbers(Pos(0, p.y + 1), "", Set.empty, if c.nonEmpty then found :+ (c.toLong -> l) else found)
        else
          if isDigit(p) then
            numbers(Pos(p.x + 1, p.y), s"$c${char(p)}", l + p, found)
          else
            numbers(Pos(p.x + 1, p.y), "", Set.empty, if c.nonEmpty then found :+ (c.toLong -> l) else found)

    case class Num(value: Long, loc: Set[Pos], adjacentSymbols: Set[(Char,Pos)])

    val numbersWithAdjacentSymbols: Vector[(Long,Set[Pos])] =
      numbers().filter((_,l) => l.exists(hasAdjacentSymbol))

    def gearRatio(p: Pos): Option[Long] =
      assert(char(p) == '*')
      val numbers = numbersWithAdjacentSymbols.filter((_,l) => l.flatMap(adjacents).contains(p))
      Option.when(numbers.size == 2){
        val (n0, _) = numbers(0)
        val (n1, _) = numbers(1)
        n0 * n1
      }

    def gearRatios: Vector[Long] =
      gears.flatMap((p,_) => gearRatio(p)).toVector

  val start1: Long =
    System.currentTimeMillis

  val input: Page =
    Page(Source.fromResource(s"input$day.txt").getLines.map(_.toVector).toVector)

  val answer1: Long =
    input.numbersWithAdjacentSymbols.map(_._1).sum

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    input.gearRatios.sum

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
