package aoc2024

import nmcb.*
import nmcb.pos.*

import scala.annotation.*

object Day14 extends AoC:

  case class Robot(p: Pos, v: Pos)

  val space: Space =
    val robots: Vector[Robot] = lines.map:
      case s"p=$px,$py v=$vx,$vy" => Robot(Pos(px.toInt, py.toInt), Pos(vx.toInt, vy.toInt))

    Space(robots, sizeX = 101, sizeY = 103)

  case class Space(robots: Vector[Robot], sizeX: Int, sizeY: Int):

    lazy val robotsByPos: Map[Pos,Vector[Robot]] =
      robots.groupMap(_.p)(identity)

    def render(p: Pos): String =
      robotsByPos.get(p).map(_.size.toString).getOrElse(".")

    override def toString: String =
      tiles.grouped(sizeX).map(_.map(render).mkString("")).mkString("\n")

    def tiles: IndexedSeq[Pos] =
      for {
        y <- 0 until sizeY
        x <- 0 until sizeX
      } yield Pos(x, y)

    def move(r: Robot): Robot =
      val n = r.p + r.v
      val nx = if n.x < 0 then sizeX + n.x else if n.x >= sizeX then n.x - sizeX else n.x
      val ny = if n.y < 0 then sizeY + n.y else if n.y >= sizeY then n.y - sizeY else n.y
      Robot(Pos(nx, ny), r.v)

    def next: Space =
      copy(robots = robots.map(move))

    def safetyFactor: Long =

      def robotsIn(min: Pos, max: Pos): Vector[Robot] =
        robots.filter(r => r.p.x >= min.x & r.p.x <= max.x & r.p.y >= min.y & r.p.y <= max.y)

      val mid = Pos(sizeX / 2, sizeY / 2)
      val robotsInQ1: Vector[Robot] = robotsIn(Pos(0, 0), Pos(mid.x - 1, mid.y - 1))
      val robotsInQ2: Vector[Robot] = robotsIn(Pos(mid.x + 1, 0), Pos(sizeX - 1, mid.y - 1))
      val robotsInQ3: Vector[Robot] = robotsIn(Pos(0, mid.y + 1), Pos(mid.x - 1, sizeY - 1))
      val robotsInQ4: Vector[Robot] = robotsIn(Pos(mid.x + 1, mid.y+1), Pos(sizeX - 1, sizeY - 1))
      List(robotsInQ1, robotsInQ2, robotsInQ3, robotsInQ4).map(_.size.toLong).product

    def robotClusters: Set[Set[Pos]] =

      @tailrec
      def cluster(todo: List[Pos], inside: Set[Pos], found: Set[Pos] = Set.empty): Set[Pos] =
        todo match
          case Nil =>
            found
          case p :: rest =>
            if inside.contains(p) then
              cluster(rest ++ p.adjoint, inside - p, found + p)
            else
              cluster(rest, inside - p, found)

      @tailrec
      def loop(robots: List[Pos], inside: Set[Pos], clusters: Set[Set[Pos]] = Set.empty): Set[Set[Pos]] =
        robots match
          case Nil =>
            clusters
          case robot :: rest =>
            val c = cluster(List(robot), inside)
            loop(rest, inside -- c, clusters + c)

      loop(robotsByPos.keys.toList, robotsByPos.keySet)

  lazy val answer1: Long =
    (0 until 100).foldLeft(space)((s,_) => s.next).safetyFactor

  lazy val answer2: Int =
    val (_, iterations) = Iterator
      .iterate(space)(_.next)
      .zipWithIndex
      .find((s,t) => s.robotClusters.exists(_.size > 50))
      .get
    iterations
