import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object Day23 extends App:

  val day: String =
    this.getClass.getName.drop(3).init

  type Maze[A] = Vector[Vector[A]]

  extension [A](g: Maze[A])

    def apply(p: Pos): A =
      g(p.y)(p.x)

    def includes(p: Pos): Boolean =
      p.x >= 0 && p.y >= 0 && p.x < g.head.size && p.y < g.size

  case class Input(maze: Maze[Char]):

    val maxX: Int = maze.head.size
    val maxY: Int = maze.size

    val start: Pos  = Pos(1, 0)
    val finish: Pos = Pos(maxX - 2, maxY - 1)

    val nodes =
      val result =
        for
          y <- 1 until maxY - 1
          x <- 1 until maxX - 1
          pos = Pos(x, y)
          if maze(pos) != '#'
          if Pos.directions.count(dir => maze(pos + dir) != '#') > 2
        yield pos
      result.toSet + start + finish

    case class Trail(from: Pos, path: Set[Pos])

    object Trail:
      def startFrom(p: Pos): Trail =
        Trail(p, Set(p))

    def directions(c: Char): Pos =
      c match
        case '^' => Pos(0, -1)
        case '>' => Pos(1, 0)
        case 'v' => Pos(0, 1)
        case '<' => Pos(-1, 0)

    def solve(icy: Boolean): Int =

      def reachable(from: Pos)(target: Pos): Set[Pos] =
        if target != from && nodes(target) then
          Set.empty
        else
          val dirs =
            if !icy || maze(target) == '.' then
              Pos.directions
            else
              Set(directions(maze(target)))
          for
            dir <- dirs
            pos = target + dir
            if maze.includes(pos)
            if maze(pos) != '#'
          yield pos

      val neighbourhood: Map[Pos, Map[Pos, Int]] =
        nodes
          .map(from => from -> Dijkstra.traverse(from, reachable(from)).filter(p => p._1 != from && nodes(p._1)))
          .toMap

      def neighbouring(trail: Trail): Iterator[(Trail, Int)] =
        if (trail.from != finish)
          for
            (to, dist) <- neighbourhood(trail.from).iterator
            if !trail.path.contains(to)
          yield Trail(to, trail.path + to) -> dist
        else
          Iterator.empty

      @tailrec
      def loop(todo: Set[Trail], visited: Map[Trail, Int]): Map[Trail, Int] =
        if todo.isEmpty then
          visited.filter((trail, _) => trail.path.contains(finish))
        else
          val trail = todo.head
          val (left, updated) =
            neighbouring(trail).foldLeft((todo - trail, visited)):
              case ((todo, visited), (neighbour, steps)) =>
                val distance = visited(trail) + steps
                if visited.contains(neighbour) then
                  if visited(neighbour) < distance then
                    (todo + neighbour, visited + (neighbour -> distance))
                  else
                    (todo, visited)
                else
                  (todo + neighbour, visited + (neighbour -> distance))
          loop(left, updated)

      val trail: Trail = Trail.startFrom(start)
      loop(Set(trail), Map(trail -> 0)).values.max

  def input(postfix: String = "-test"): Input =
    Input(
      Source
        .fromResource(s"input$day$postfix.txt")
        .getLines
        .map(_.toVector)
        .toVector)

  val start1: Long = System.currentTimeMillis
  val answer1: Int = input().solve(icy = true)
  println(s"Answer day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Int = input().solve(icy = false)
  println(s"Answer day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")

  case class Pos(x: Int, y: Int):
    def +(that: Pos): Pos =
      Pos(x + that.x, y + that.y)

    def neighbours: Set[Pos] =
      Pos.directions.map(_ + this)

  object Pos:
    val directions: Set[Pos] =
      Set(Pos(-1, 0), Pos(1, 0), Pos(0, -1), Pos(0, 1))

  object Dijkstra:
    def traverse[A](start: A, reachable: A => Set[A]): Map[A, Int] =
      val found = mutable.Map.empty[A, Int]
      val todo  = mutable.Queue.empty[(Int, A)]

      todo.enqueue((0, start))

      while todo.nonEmpty do
        val (dist, node) = todo.dequeue()
        if !found.contains(node) then
          found(node) = dist

          def process(newNode: A): Unit =
            if !found.contains(newNode) then
              val newDist = dist + 1
              todo.enqueue((newDist, newNode))

          reachable(node).iterator.foreach(process)

      found.toMap

