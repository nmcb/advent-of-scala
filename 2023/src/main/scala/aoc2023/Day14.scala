package aoc2023

import nmcb.*

import scala.annotation.tailrec

object Day14 extends AoC:

  case class Grid(image: Vector[Vector[Char]]):

    override def toString: String =
      image.map(_.mkString).mkString("\n", "\n", "\n")

    private def tiltL(row: Vector[Char]): Vector[Char] =
      @tailrec
      def loop(todo: Vector[Char], acc: Vector[Char] = Vector.empty): Vector[Char] =
        if todo.isEmpty then
          acc
        else if todo.head == '#' then
          loop(todo.tail, acc :+ '#')
        else
          val section = todo.takeWhile(_ != '#')
          val rounds  = section.count(_ == 'O')
          val empties = section.count(_ == '.')
          val aligned = section.foldLeft(Seq.empty[Char])((a,p) => Seq.fill(rounds)('O') ++ Seq.fill(empties)('.'))
          loop(todo.drop(section.length), acc ++ aligned)

      loop(row)

    lazy val tiltN: Grid =
      Grid(image.transpose.map(tiltL).transpose)

    lazy val tiltE: Grid =
      Grid(image.map(l => tiltL(l.reverse).reverse))

    lazy val tiltS: Grid =
      Grid(image.transpose.map(l => tiltL(l.reverse).reverse).transpose)

    lazy val tiltW: Grid =
      Grid(image.map(tiltL))

    lazy val cycle: Grid =
      tiltN.tiltW.tiltS.tiltE

    lazy val load: Int =
      image.reverse.zipWithIndex.foldLeft(0):
        case (a,(r,i)) => a + r.count(_ == 'O') * (i + 1)

    def cycleInvariant: Grid =
      this


  lazy val grid: Grid = Grid(lines.map(_.toVector))
  
  
  lazy val answer1: Long = grid.tiltN.load
  lazy val answer2: Long = Cycle.find(grid, _.cycle, _.cycleInvariant).simulate(1_000_000_000L).load
