package aoc2021

import nmcb.*
import scala.annotation.tailrec

object Day25 extends AoC:

  case class Pos(x: Int, y: Int):
    
    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

  case class Floor(tiles: Map[Pos,Char], sizeX: Int, sizeY: Int):

    def eastFacing: Set[Pos] =
      tiles.filter((p,c) => c == '>').keySet
    
    def southFacing: Set[Pos] =
      tiles.filter((p,c) => c == 'v').keySet
    
    def eastOf(p: Pos): Pos =
      if p.x < sizeX - 1 then p + Pos(1,0) else p.copy(x = 0)
    
    def southOf(p: Pos): Pos =
      if p.y < sizeY - 1 then p + Pos(0,1) else p.copy(y = 0)

    def turn: Floor =
      val (eastMove, eastRemoved) = tiles.partition((_,c) => c == '>')
      val eastMoved   = eastMove.map((p,c)    => if tiles(eastOf(p)) == '.' then p -> '.' else p -> c)
      val eastUpdated = eastMove.filter((p,c) => tiles(eastOf(p)) == '.').map((p,c) => eastOf(p) -> c)
      val eastTiles   = eastRemoved ++ eastUpdated ++ eastMoved

      val (southMove, southRemoved) = eastTiles.partition((_,c) => c == 'v')
      val southMoved   = southMove.map((p,c)    => if eastTiles(southOf(p)) == '.' then p -> '.' else p -> c)
      val southUpdated = southMove.filter((p,c) => eastTiles(southOf(p)) == '.').map((p,c) => southOf(p) -> c)
      val southTiles   = southRemoved ++ southUpdated ++ southMoved
      copy(tiles = southTiles)


  val floor: Floor =
    val sizeX = lines.head.size
    val sizeY = lines.size
    val tiles = Vector.tabulate(sizeX, sizeY)((x, y) => Pos(x, y) -> lines(y)(x)).flatten.toMap
    Floor(tiles, sizeX, sizeY)

  @tailrec
  def solve(floor: Floor, i: Int = 1): Int =
    val next = floor.turn
    if floor == next then
      i
    else
      solve(next, i + 1)

  lazy val answer1: Int    = solve(floor)
  lazy val answer2: String = "<unimplemented>"
