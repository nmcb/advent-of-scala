package aoc2021

import nmcb.*

import scala.annotation.tailrec

object Day09 extends AoC:

  val floor = Floor(lines.toList.map(line => line.toList.map(_.toString.toInt)))

  type Pos = (Int,Int)

  extension (p: (Int,Int))
    def x: Int = p._1
    def y: Int = p._2

  case class Floor(loc: List[List[Int]]):
    val sizeX: Int = loc.head.size
    val sizeY: Int = loc.size

    def sample(x: Int, y: Int): Option[Int] =
      Option.when(x >= 0 && x < sizeX && y >= 0 && y < sizeY)(loc(y)(x))

    def height(x: Int, y: Int): Int =
      sample(x,y).getOrElse(sys.error(s"index out of bounds: ($x,$y)"))

    def neighbourHeights(x: Int, y: Int): List[(Pos,Int)] =
      List((1,0),(-1,0),(0,1),(0,-1))
        .map((nx,ny) => (x + nx, y + ny))
        .flatMap(p => sample(p.x, p.y).map(h => p -> h))

    def upstream(x: Int, y: Int): List[Pos] =
      neighbourHeights(x,y)
        .filter((_,nh) => nh < 9 && nh > height(x,y))
        .map((n,_) => n)

  val heights: List[((Int, Int), Int)] =
    var result: List[(Pos,Int)] = List.empty
    for
      x <- 0 until floor.sizeX
      y <- 0 until floor.sizeY
    do
      val height = floor.height(x,y)
      if floor.neighbourHeights(x,y).forall((_,nh) => height < nh) then
        result = ((x,y), height) :: result
    result

  lazy val answer1: Int =
    heights.map((_, h) => h + 1).sum


  lazy val answer2: Int =
    @tailrec
    def loop(todo: List[Pos], acc: List[Pos] = List.empty): Int =
      if todo.isEmpty then
        acc.size
      else
        val (x,y) = todo.head
        val upstream = floor.upstream(x,y)
        loop(upstream ++ todo.tail, ((x,y) :: acc).distinct)
    
    heights
      .map((point,_) => loop(List((point.x,point.y))))
      .sorted
      .reverse
      .take(3)
      .product
