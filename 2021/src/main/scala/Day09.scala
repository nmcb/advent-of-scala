import scala.io.*

object Day09 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val floor =
    Floor(
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .toList
        .map(line => line.toList.map(_.toString.toInt))
    )

  type Pos = (Int,Int)

  extension (p: (Int,Int))
    def x: Int = p._1
    def y: Int = p._2

  case class Floor(loc: List[List[Int]]):
    val sizeX = loc.head.size
    val sizeY = loc.size

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

  val heights =
    var result: List[(Pos,Int)] = List.empty
    for
      x <- 0 until floor.sizeX
      y <- 0 until floor.sizeY
    do
      val height = floor.height(x,y)
      if floor.neighbourHeights(x,y).forall((_,nh) => height < nh) then
        result = ((x,y), height) :: result
    result

  val start1  = System.currentTimeMillis
  lazy val answer1 = heights.map((_,h) => h + 1).sum
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  val start2  = System.currentTimeMillis
  lazy val answer2 =
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

  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
