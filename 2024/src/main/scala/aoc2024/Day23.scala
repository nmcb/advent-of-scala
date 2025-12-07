package aoc2024

import nmcb.*

/** @see Credits - https://github.com/sim642 */
object Day23 extends AoC:

  type N = String

  lazy val edges: Set[(String, String)] =

    val directed = lines.toSet.map:
      case s"$a-$b" => (a,b)

    directed ++ directed.map(_.swap)

  lazy val neighbours: Map[N,Set[N]] =
    edges.groupMap(_._1)(_._2)

  lazy val solve1: Set[Set[N]] =
    for
      (a,b) <- edges
      c     <- neighbours(a) intersect neighbours(b)
      if a.startsWith("t") || b.startsWith("t") || c.startsWith("t")
    yield
      Set(a,b,c)

  lazy val answer1: Long = solve1.size
  lazy val answer2: String = BronKerbosch.run(neighbours).toVector.sorted.mkString(",")
