package aoc2021

import nmcb.*

import scala.annotation.tailrec
import scala.util.matching.Regex

object Day04 extends AoC:

  val draws: Vector[Int] =
    lines
      .head
      .split(",")
      .map(_.toInt)
      .toVector

  val Line: Regex = """\s*(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s*""".r

  def boards: Vector[Board[Int]] =
    lines
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

  @tailrec
  def playWhoWinsLast[A](draws: Vector[A], game: Vector[Board[A]] = boards): Board[A] =
    val round = game.map(_.draw(draws.head))
    val todo  = round.filterNot(_.hasBingo)
    if todo.nonEmpty then
      playWhoWinsLast(draws.tail, todo)
    else 
      round.filter(_.lastDraw == draws.head).head


  lazy val board1: Board[Int] = playWhoWinsFirst(draws)
  lazy val answer1: Int       = board1.unmarked.sum * board1.lastDraw

  lazy val board2: Board[Int] = playWhoWinsLast(draws)
  lazy val answer2: Int       = board2.unmarked.sum * board2.lastDraw
