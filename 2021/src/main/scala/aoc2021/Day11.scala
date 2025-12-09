package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day11 extends AoC:

  type Octo = (Int,Int)

  extension (octo: Octo)
    def x = octo._1
    def y = octo._2

  val config: Map[Octo,Int] =
    lines
      .zipWithIndex.flatMap: (row,y) =>
        row.zipWithIndex.map: (level,x) =>
          (x,y) -> level.toString.toInt
      .toMap

  object Octopuses:

    def make(input: Map[Octo,Int]): Octopuses =
      def neighboursOf(octo: Octo): List[Octo] =
        List((-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1))
          .filter(n => input.keySet.contains((octo.x + n.x, octo.y + n.y)))
          .map(n => (octo.x + n.x, octo.y + n.y))
      Octopuses(energies = input, neighbours = input.keys.map(octo => octo -> neighboursOf(octo)).toMap)


  case class Octopuses(energies: Map[Octo,Int], neighbours: Map[Octo,List[Octo]], flashed: Int = 0):

    private def normaliseEnergies: Octopuses =
      copy(energies = energies.map((pos, l) => if l > 9 then pos -> 0 else pos -> l))

    def step: Octopuses =
      @tailrec
      def loop(flashes: List[Octo], acc: Map[Octo, Int], flashed: List[Octo] = List.empty): Octopuses =
        if flashes.isEmpty then
          copy(energies = acc, flashed = flashed.size)
        else if flashed.contains(flashes.head) then
          loop(flashes.tail, acc, flashed)
        else
          val pos = flashes.head
          val update = neighbours(pos).map(octo => octo -> (acc(octo) + 1)).toMap
          val propagate = update.filter((_, level) => level > 9).keySet.toList
          loop(propagate ++ flashes.tail, acc ++ update, pos :: flashed)

      val updated = energies.map((pos,l) => pos -> (l + 1))
      val flashes = updated.filter((_,l) => l > 9).keySet.toList.sorted
      loop(flashes, updated).normaliseEnergies

  val octopuses: Octopuses = Octopuses.make(config)


  lazy val answer1: Int = Iterator.iterate(octopuses)(_.step).take(101).map(_.flashed).sum
  lazy val answer2: Int = Iterator.iterate(octopuses)(_.step).takeWhile(_.flashed != 100).size
