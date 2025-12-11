package aoc2020

import nmcb.*
import scala.annotation.tailrec

object Day11 extends AoC:

  type Floor = Vector[String]

  extension (floor: Floor)
    
    def sizeX: Int = floor.head.length
    def sizeY: Int = floor.size

    def within(x: Int, y: Int): Boolean =
      x >= 0 && x < sizeX && y >= 0 && y < sizeY

    def seat(x: Int, y: Int): Option[Char] =
      Option.when(within(x,y))(floor(y)(x)).filter(c => c == 'L' || c == '#')

    def count1(x: Int, y: Int): Int =
      val N  = seat(x    , y - 1)
      val E  = seat(x + 1, y    )
      val S  = seat(x    , y + 1)
      val W  = seat(x - 1, y    )
      val NE = seat(x + 1, y - 1)
      val NW = seat(x - 1, y - 1)
      val SE = seat(x + 1, y + 1)
      val SW = seat(x - 1, y + 1)
      List(NW,N,NE,E,SE,S,SW,W).flatten.count(_ == '#')

    def count2(x: Int, y: Int): Int =
      @tailrec
      def find(x: Int, y: Int, dx: Int, dy: Int): Option[Char] =
        if !within(x, y) then
          None
        else
          seat(x + dx, y + dy) match
            case None    => find(x + dx, y + dy, dx, dy)
            case Some(c) => Some(c)

      val N  = find(x, y,  0, -1)
      val E  = find(x, y, +1,  0)
      val S  = find(x, y,  0, +1)
      val W  = find(x, y, -1,  0)
      val NE = find(x, y, +1, -1)
      val NW = find(x, y, -1, -1)
      val SE = find(x, y, +1, +1)
      val SW = find(x, y, -1, +1)
      List(NW,N,NE,E,SE,S,SW,W).flatten.count(_ == '#')

    private def nextState(max: Int, count: (Int,Int) => Int)(x: Int, y: Int): Option[Char] =
      seat(x,y) match
        case Some('L') if count(x,y) == 0   => Some('#')
        case Some('#') if count(x,y) >= max => Some('L')
        case unchanged                      => unchanged

    def nextState1: (Int,Int) => Option[Char] =
      nextState(max = 4, count = count1)

    def nextState2: (Int,Int) => Option[Char] =
      nextState(max = 5, count = count2)

    def next(step: (Int, Int) => Option[Char]): Floor =
      Vector.tabulate(sizeY,sizeX)((y,x) => step(x,y).getOrElse('.')).map(_.mkString)

    def totalOccupied: Int =
      var count = 0
      for
        y <- 0 until sizeY
        x <- 0 until sizeX
        if seat(x,y).contains('#')
      do
        count += 1
      count

  def solve(floor: Floor, step: Floor => (Int,Int) => Option[Char]): Floor =
    @tailrec
    def loop(current: Floor): Floor =
      val next = current.next(step(current))
      if next == current then current else loop(next)
    loop(floor)


  lazy val answer1: Int = solve(lines, _.nextState1).totalOccupied
  lazy val answer2: Int = solve(lines, _.nextState2).totalOccupied
