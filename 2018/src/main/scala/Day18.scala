import scala.collection.*
import scala.io.Source

import nmcb.*
import nmcb.predef.*

object Day18 extends App:

  case class Pos(x: Int, y: Int):

    infix def+(that: Pos): Pos =
      copy(x = x + that.x, y = y + that.y)

    def adjacent: Set[Pos] =
      Set(Pos(-1,-1),Pos(-1,0),Pos(-1,1),Pos(0,-1),Pos(0,1),Pos(1,-1),Pos(1,0),Pos(1,1))
        .map(_ + this)

  type Area = Map[Pos,Char]

  case class Landscape(area: Area, sizeX: Int, sizeY: Int):

    def asString: String =
      val sb = mutable.StringBuilder()
      for
        y <- 0 until sizeX
        x <- 0 until sizeX
        p = Pos(x,y)
      do
        sb.append(area(p))
      sb.toString.grouped(sizeX).mkString("\n","\n","\n")

    def wooded: Int =
      area.view.values.count(_ == '|')

    def lumberyards: Int =
      area.view.values.count(_ == '#')

    def resourceValue: Int =
      wooded * lumberyards

    def within(p: Pos): Boolean =
      p.x >= 0 && p.x < sizeX && p.y >= 0 && p.y < sizeY

    def surroundedBy(p: Pos, c: Char): Int =
      p.adjacent.filter(within).count(p => area(p) == c)

    def tick: Landscape =

      val next = mutable.ArrayBuffer.empty[(Pos,Char)]
      for
        y <- 0 until sizeY
        x <- 0 until sizeX
        p = Pos(x,y)
      do
        val c = area(p) match
          case '.' => if surroundedBy(p, '|') >= 3 then '|' else '.'
          case '|' => if surroundedBy(p, '#') >= 3 then '#' else '|'
          case '#' => if surroundedBy(p, '#') >= 1 && surroundedBy(p, '|') >= 1 then '#' else '.'

        next += (p -> c)

      copy(area = next.toMap)


  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  val landscape: Landscape =
    val lines = Source.fromResource(s"input$day.txt").getLines.toVector
    val sizeX = lines(0).size
    val sizeY = lines.size
    val area  = List.tabulate(sizeX, sizeY)((x,y) => Pos(x,y) -> lines(y)(x)).flatten.toMap
    Landscape(area, sizeX, sizeY)

  val start1  = System.currentTimeMillis
  lazy val answer1 = Iterator.iterate(landscape)(_.tick).nth(10).resourceValue
  println(s"Answer AOC 2018 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2  = System.currentTimeMillis
  lazy val answer2 = Cycle.find(landscape, _.tick).simulate(1000000000).resourceValue
  println(s"Answer AOC 2018 day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
