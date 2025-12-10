package aoc2020

import nmcb.*
import scala.annotation.tailrec


object Day03 extends AoC:

  case class Forest(tile: Vector[Vector[Char]]):
    val sizeX: Int = tile(0).size
    val sizeY: Int = tile.size

    def sample(x: Int, y: Int): Option[Char] =
      Option.when(y < sizeY)(tile(y)(x % sizeX))

    def walk(dx: Int, dy: Int): Long =
      @tailrec
      def step(x: Int, y: Int, acc: Long = 0): Long =
        sample(x, y) match
          case None      => acc
          case Some('.') => step(x + dx, y + dy, acc)
          case Some('#') => step(x + dx, y + dy, acc + 1)
          case _         => sys.error(s"boom: x=$x,y=$y,dx=$dx,dy=$dy")
      step(dx,dy)

  val forest: Forest = Forest(lines.map(_.toVector))

  val walks: List[Long] =
    List(
      forest.walk(1,1),
      forest.walk(3,1),
      forest.walk(5,1),
      forest.walk(7,1),
      forest.walk(1,2)
    )


  lazy val answer1: Long = forest.walk(3, 1)
  lazy val answer2: Long = walks.product
