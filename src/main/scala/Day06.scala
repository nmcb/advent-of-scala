import scala.annotation.tailrec
import scala.io.*

object Day06 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  case class Race(time: Int, distance: Long):
    def race(speed: Long): Long =
      val dt = time - speed
      dt * speed

    def wins =
      def loop(c: Int, a: Int = 0): Int =
        if race(c) > distance then loop(c + 1, a + 1) else if a == 0 then loop(c + 1, a) else a
      loop(0)

  val start1: Long =
    System.currentTimeMillis

  val answer1: Long =
    List(Race(45,295L),Race(98,1734L),Race(83,1278L),Race(73,1210L)).map(_.wins).product

  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long =
    System.currentTimeMillis

  val answer2: Long =
    Race(45988373,295173412781210L).wins

  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
