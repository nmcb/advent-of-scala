import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  enum Dir:
    case N, E, S, W

    infix def follow(c: Char): Dir =
      (this, c) match
        case (_, '|') | (_, '-') => this
        case (N, '/')            => E
        case (E, '/')            => N
        case (S, '/')            => W
        case (W, '/')            => S
        case (N, '\\')           => W
        case (E, '\\')           => S
        case (S, '\\')           => E
        case (W, '\\')           => N
        case _                   => sys.error(s"unable to follow dir=$this, char=$c")

    infix def turn(turn: Turn): Dir =
      (this, turn) match
        case (N, Turn.Left)     => W
        case (E, Turn.Left)     => N
        case (S, Turn.Left)     => E
        case (W, Turn.Left)     => S
        case (N, Turn.Right)    => E
        case (E, Turn.Right)    => S
        case (S, Turn.Right)    => W
        case (W, Turn.Right)    => N
        case (_, Turn.Straight) => this


  import Dir.*

  type Pos  = (Int,Int)

  extension (pos: Pos)
    def x = pos._1
    def y = pos._2

    infix def move(d: Dir): Pos =
      d match
        case N => (pos.x, pos.y - 1)
        case E => (pos.x + 1, pos.y)
        case S => (pos.x, pos.y + 1)
        case W => (pos.x - 1, pos.y)

  type Grid = Vector[Vector[Char]]

  extension (grid: Grid)
    def sizeX = grid(0).size
    def sizeY = grid.size
    def charAt(p: Pos): Option[Char] = grid.lift(p.y).flatMap(_.lift(p.x))

  enum Turn:
    case Left, Straight, Right

    def next: Turn =
      this match
        case Left     => Straight
        case Straight => Right
        case Right    => Left

  case class Cart(pos: Pos, dir: Dir, atIntersection: Turn = Turn.Left):

    def move(grid: Grid): Cart =
      val c = grid.charAt(pos move dir)
      c match
        case Some('|') | Some('-')  => copy(pos = pos move dir)
        case Some('/') | Some('\\') => copy(pos = pos move dir, dir = dir follow c.get)
        case Some('+')              => copy(pos = pos move dir, dir = dir turn atIntersection, atIntersection = atIntersection.next)
        case _                      => sys.error(s"unexpected char at pos=$pos, char=$c")

  val (grid: Grid, carts: Vector[Cart]) =
    val lines =
      Source
        .fromResource(s"input$day.txt")
        .getLines
        .map(_.toVector)
        .toVector

    val carts =
      for
        y <- (0 until lines.sizeY).toVector
        x <- (0 until lines.sizeX).toVector
      yield
        lines.charAt((x,y)) match
          case Some(c) if c == '^' => Some(Cart((x,y), N))
          case Some(c) if c == '>' => Some(Cart((x,y), E))
          case Some(c) if c == 'v' => Some(Cart((x,y), S))
          case Some(c) if c == '<' => Some(Cart((x,y), W))
          case _                   => None

    val grid =
      lines.map(_.map:
        case '^' | 'v' => '|'
        case '<' | '>' => '-'
        case c         => c
      )

    (grid , carts.flatten)

  extension (posCount : (Pos,Int))
    def pos: Pos   = posCount._1
    def count: Int = posCount._2

  def solve1(grid: Grid, carts: Vector[Cart]): String =

    /**
     * Don't move all carts per iteration, or we'll miss collisions that pass each other, e.g.:
     *
     * # step 1
     * --><--
     *
     * # step 2
     * --<>--
     *
     */
    @tailrec
    def go(todo: Vector[Cart], done: Vector[Cart] = Vector.empty): Pos =
      val positionCount = (todo ++ done).groupMapReduce(_.pos)(_ => 1)(_ + _)
      if positionCount.exists(_.count > 1) then
        positionCount.maxBy(_.count).pos
      else if todo.isEmpty then
        go(todo = done.sortBy(_.pos))
      else
        go(todo = todo.tail, done = todo.head.move(grid) +: done)

    val collision = go(carts.sortBy(_.pos))
    s"${collision.x},${collision.y}"

  val start1  = System.currentTimeMillis
  val answer1 = solve1(grid, carts)
  println(s"Day $day answer part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  def solve2(grid: Grid, carts: Vector[Cart]): String =

    @tailrec
    def go(todo: Vector[Cart], done: Vector[Cart] = Vector.empty): Pos =
      if (todo ++ done).size == 1 then
        /** don't forget to move the last cart one tick */
        (todo ++ done).head.move(grid).pos
      else if todo.isEmpty then
        go(todo = done.sortBy(_.pos))
      else
        val moved = todo.head.move(grid)
        val positionCount = (moved +: (todo.tail ++ done)).groupMapReduce(_.pos)(_ => 1)(_ + _)
        positionCount.find(_.count > 1) match
          case Some(pos,_) => go(todo = todo.tail.filterNot(_.pos == pos), done = done.filterNot(_.pos == pos))
          case None        => go(todo = todo.tail, done = moved +: done)

    val last = go(carts.sortBy(_.pos))
    s"${last.x},${last.y}"

  val start2  = System.currentTimeMillis
  val answer2 = solve2(grid, carts)
  println(s"Day $day answer part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
