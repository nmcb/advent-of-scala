import nmcb.*

import Dijkstra.*

import scala.io.*

object Day20 extends App:

  val day: String =
    getClass.getName.drop(3).init

  case class Design(stripes: String)

  case class Program(start: Pos, end: Pos, track: Grid[Char]):

    def run: Int =
      val graph = Graph.fromGrid(track, '.')
      val max   = Dijkstra.run(graph, start).distanceTo(end).get
      var count = 1
      val cheats =
        cheatsFrom(graph)
          .flatMap((to, from, patch) =>
            println(s"$count")
            count += 1
            Dijkstra
              .run(patch, start).distanceTo(end)
              .map(time => ((to, from), max - time)))

      val saves = cheats.groupMap(_._2)(_._1)
      saves.filter((save, cheats) => save >= 100).map(_._2.size).sum

    def cheatsFrom(graph: Graph[Pos]): Set[(Pos, Pos, Graph[Pos])] =
      def passes(p: Pos, d: Dir): Option[(Pos,Pos)] =
        val step1 = p.move(d)
        val step2 = p.move(d).move(d)
        track.peekOption(step1) match
          case Some('#') =>
            track.peekOption(step2) match
              case Some('.') => Option(p, step2)
              case _         => None
          case _ => None

      val x = track.positions.flatMap(p => Dir.values.flatMap(d => passes(p, d))).map((f,t) =>
        Edge(f, t, 2)).map(e => (e.from, e.to, graph.add(e)))
      println(s"size=${x.size}")
      x




  object Program:
    def make(track: Grid[Char]): Program =
      val start = track.findOne('S')
      val end   = track.findOne('E')
      Program(start, end, track.updated(start, '.').updated(end, '.'))




  val racetrack = Grid.fromLines(Source.fromResource(s"input$day.txt").getLines)
  println(racetrack.asString)

  val start1: Long  = System.currentTimeMillis
  val answer1: Long = Program.make(racetrack).run
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
