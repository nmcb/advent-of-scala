import nmcb.*
import predef.*

import scala.io.*

/** credits simon saan - https://sim642.eu/ */
object Day23 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  type N = String

  lazy val edges =
    val directed =
      Source.fromResource(s"input$day.txt").getLines.map:
        case s"$a-$b" => (a,b)
      .toSet
    directed ++ directed.map(_.swap)


  lazy val neighbours: Map[N,Set[N]] =
    edges.groupMap(_._1)(_._2)

  lazy val solve1 =
    for {
      (a,b) <- edges
      c     <- neighbours(a) intersect neighbours(b)
      if a.startsWith("t") || b.startsWith("t") || c.startsWith("t")
    } yield
      Set(a,b,c)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = solve1.size
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long    = System.currentTimeMillis
  val answer2: String = BronKerbosch.run(neighbours).toVector.sorted.mkString(",")
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
