import scala.io.*

object Day04 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val draws: Vector[Int] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .next
      .split(",")
      .map(_.toInt)
      .toVector

  val Line =
    """\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*""".r

  def boards: Vector[Board[Int]] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .drop(1)
      .filterNot(_.isBlank)
      .grouped(5)
      .map: lines =>
        lines.foldLeft(Board.empty[Int]):
          case (board, Line(n0,n1,n2,n3,n4)) =>
            board.addRow(Vector(n0.toInt,n1.toInt,n2.toInt,n3.toInt,n4.toInt))
          case (_,line) =>
            sys.error(s"unable to read line=$line")
      .toVector

  case class Board[A](rows: Vector[Vector[A]], draws: Vector[A] = Vector.empty):

    def addRow(row: Vector[A]): Board[A] =
      copy(rows = rows :+ row)

    def draw(num: A): Board[A] =
      if !hasBingo then copy(draws = draws :+ num) else this

    def hasBingo: Boolean =
      hasRowWith(draws) || hasColumnWith(draws)

    def unmarked: Vector[A] =
      rows.flatten.filterNot(draws.contains)

    def lastDraw: A =
      draws.last

    private def hasRowWith(draws: Vector[A]): Boolean =
      rows.exists(_.forall(draws.contains))

    private def hasColumnWith(draws: Vector[A]): Boolean =
      rows.transpose.exists(_.forall(draws.contains))


  object Board:

    def empty[A]: Board[A] =
      Board[A](Vector.empty)

  def playWhoWinsFirst[A](draws: Vector[A], boards: Vector[Board[A]] = boards): Board[A] =
    val round = boards.map(_.draw(draws.head))
    round
      .find(_.hasBingo)
      .getOrElse(playWhoWinsFirst(draws.tail, round))

  val start1  = System.currentTimeMillis
  val board1  = playWhoWinsFirst(draws)
  lazy val answer1 = board1.unmarked.sum * board1.lastDraw
  println(s"Answer AOC 2021 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  def playWhoWinsLast[A](draws: Vector[A], game: Vector[Board[A]] = boards): Board[A] =
    val round = game.map(_.draw(draws.head))
    val todo  = round.filterNot(_.hasBingo)
    if todo.nonEmpty then
      playWhoWinsLast(draws.tail, todo)
    else 
      round.filter(_.lastDraw == draws.head).head

  val start2  = System.currentTimeMillis
  val board2  = playWhoWinsLast(draws)
  lazy val answer2 = board2.unmarked.sum * board2.lastDraw
  println(s"Answer AOC 2021 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
