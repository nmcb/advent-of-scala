package aoc2023

import nmcb.*

import scala.collection.mutable

/** @see Credits - https://github.com/sanraith */
object Day23 extends AoC:

  def part1(input: String): Int =
    val (start, end, maze) = parseMaze(input)
    val queue = mutable.Queue((start, S, 0))
    val visited = mutable.Map.empty[Pos, Int]
    while queue.nonEmpty do
      val (pos, dir, dist) = queue.dequeue()
      visited.get(pos) match
        case Some(prevDist) if prevDist >= dist => ()
        case _ =>
          visited(pos) = dist
          val nextDirs = maze.grid(pos) match
            case '>' => Seq(E)
            case 'v' => Seq(S)
            case '<' => Seq(W)
            case '^' => Seq(N)
            case '.' => Dir
            case _ => throw new Exception(s"input error: pos=$pos")
          nextDirs
            .map(d => (pos + d, d))
            .filter((p, d) => maze.grid.contains(p) && -d != dir)
            .foreach((p, d) => queue.enqueue((p, d, dist + 1)))
    visited(end)

  def part2(input: String): Int =
    val (startPoint, endPoint, maze) = parseMaze(input)
    val nodes = parseNodes(startPoint, endPoint, maze)
    val start = nodes(startPoint)
    val end   = nodes(endPoint)

    var longest = 0
    val queue = mutable.Queue((start, Set.empty[Node], 0))
    while queue.nonEmpty do
      val (node, path, dist) = queue.dequeue()
      if node == end then
        longest = math.max(longest, dist)
      else
        val pathNext = path + node
        node.edges
          .filter((d, n) => !pathNext.contains(n))
          .foreach((d, n) => queue.enqueue((n, pathNext, dist + d)))
    longest

  def parseNodes(start: Pos, end: Pos, layout: Maze) =
    val queue = mutable.Queue((start, 0, S, None: Option[Node]))
    val nodes = mutable.Map.empty[Pos, Node]

    while queue.nonEmpty do
      val (pos, dist, dir, fromNode) = queue.dequeue()
      val nextPair =
        if Dir.map(pos + _).count(layout.grid.contains) != 2 then
          val newNode = !nodes.contains(pos)
          val node = nodes.getOrElseUpdate(pos, Node(pos))
          fromNode.foreach: fromNode =>
            fromNode.edges.addOne(dist, node)
            node.edges.addOne(dist, fromNode)
          if newNode then Some(1, Some(node)) else None
        else Some(dist + 1, fromNode)
      nextPair.foreach: (nextDist, nextFrom) =>
        Dir
          .map(d => (pos + d, d))
          .filter: (p, d) =>
            layout.grid.contains(p) && -d != dir && fromNode.forall(f => f.pos != p)
          .foreach((p, d) => queue.enqueue((p, nextDist, d, nextFrom)))

    nodes.toMap

  def parseMaze(input: String) =
    val width = input.linesIterator.next.length
    val height = input.linesIterator.length
    val grid = input.linesIterator.zipWithIndex
      .flatMap: (l, y) =>
        l.zipWithIndex
          .map((c, x) => Pos(x, y) -> c)
          .filter((_, c) => c != '#')
          .map((p, c) => p -> c)
      .toMap
    val sortedPath = grid.keySet.toSeq.sortBy(p => (p.y, p.x))
    val start = sortedPath.head
    val end = sortedPath.last
    (start, end, Maze(grid, width, height))

  case class Maze(grid: Map[Pos, Char], width: Int, height: Int)

  case class Node(pos: Pos):
    val edges = mutable.Map.empty[Int, Node]

  case class Pos(x: Int, y: Int):
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)
    def -(other: Pos): Pos = Pos(x - other.x, y - other.y)
    def *(other: Pos): Pos = Pos(x * other.x, y * other.y)
    def *(scalar: Int): Pos = Pos(x * scalar, y * scalar)
    def manhattan(other: Pos): Int = (x - other.x).abs + (y - other.y).abs
    def unary_- : Pos = Pos(-x, -y)

  type Dir = Pos
  val E: Dir = Pos(1, 0)
  val S: Dir = Pos(0, 1)
  val W: Dir = Pos(-1, 0)
  val N: Dir = Pos(0, -1)
  val Dir: Seq[Dir] = Seq(E, S, W, N)


  lazy val answer1: Int = part1(input)
  lazy val answer2: Int = part2(input)
