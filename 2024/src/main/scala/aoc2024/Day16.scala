package aoc2024

import nmcb.*
import nmcb.predef.*

object Day16 extends AoC:

  enum Dir:
    case N, E, S, W

    def cw: Dir =
      this match
        case N => E
        case E => S
        case S => W
        case W => N

    def ccw: Dir =
      this match
        case N => W
        case W => S
        case S => E
        case E => N

  import Dir.*

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Pos =
      d match
        case N => copy(y = y - 1)
        case E => copy(x = x + 1)
        case S => copy(y = y + 1)
        case W => copy(x = x - 1)

  case class Tracer(cost: Long, pos: Pos, dir: Dir, path: Set[Pos]):
    def options: List[Tracer] =
      List(
        Tracer(cost + 1, pos.move(dir), dir, path + pos.move(dir)),
        Tracer(cost + 1000, pos, dir.cw, path),
        Tracer(cost + 1000, pos, dir.ccw, path)
      )

  case class Maze(maze: Vector[Vector[Char]]):
    val sizeX: Int = maze.head.size
    val sizeY: Int = maze.size

    override def toString: String =
      maze.map(_.mkString("")).mkString("\n")

    def within(p: Pos): Boolean =
      p.x >= 0 & p.y >= 0 & p.x < sizeX & p.y < sizeY

    def peek(p: Pos): Option[Char] =
      Option.when(within(p))(maze(p.y)(p.x))

    lazy val positions: Vector[(Pos,Char)] =
      for {
        (l, y) <- maze.zipWithIndex
        (c, x) <- l.zipWithIndex
      } yield (Pos(x, y), c)

    def find(f: Char => Boolean): Vector[Pos] =
      positions.filter((p, c) => f(c)).map((p, _) => p)

    lazy val start: Pos       = find(_ == 'S').head
    lazy val end: Pos         = find(_ == 'E').head
    lazy val walls: Set[Pos]  = find(_ == '#').toSet
    lazy val inside: Set[Pos] = find(_ == '.').toSet

    type Costs = Map[(Pos,Dir),Long]

    def advance(tracer: Tracer, costs: Costs): Boolean =
      val inside = !walls.contains(tracer.pos)
      val better = costs.get(tracer.pos -> tracer.dir).forall(_ >= tracer.cost)
      inside & better

    def solve1: Long =
      case class State(tracers: Vector[Tracer], costs: Costs = Map.empty):
        def next: State =
          tracers.foldLeft(copy(tracers = Vector.empty)): (state, tracer) =>
            if advance(tracer, state.costs) then
              val ts = state.tracers ++ tracer.options
              val cs = state.costs + (tracer.pos -> tracer.dir -> tracer.cost)
              State(tracers = ts, costs = cs)
            else
              state

        def solution: Option[Long] =
          Option.when(tracers.isEmpty)(cost)

        def cost: Long =
          Dir.values.flatMap(d => costs.get(end -> d)).min

      val tracer = Tracer(0, start, Dir.E, Set(start))
      val state  = State(Vector(tracer))
      Iterator.iterate(state)(_.next).findMap(_.solution)


    def solve2: Long =
      case class Best(cost: Long, path: Set[Pos])
      case class State(tracers: Vector[Tracer], costs: Costs, best: Best):

        def update(t: Tracer, cs: Costs, b: Best): Best =
          if      t.pos != end | t.cost > b.cost then b
          else if t.cost < b.cost                then Best(t.cost, t.path)
          else                                        b.copy(path = b.path ++ t.path)

        def next: State =
          tracers.foldLeft(copy(tracers = Vector.empty[Tracer])):
            case (state, tracer) =>
              val b  = update(tracer, state.costs, state.best)
              val ts = tracer.options.filter(t => advance(t, state.costs))
              val cs = state.costs ++ ts.map(next => next.pos -> next.dir -> next.cost)
              State(tracers = state.tracers ++ ts, costs = cs, best = b)

        def solution: Option[Long] =
          Option.when(tracers.isEmpty)(best.path.size)

      val tracers = Vector(Tracer(0, start, Dir.E, Set(start)))
      val costs   = Map(start -> Dir.E -> 0L)
      val best    = Best(Long.MaxValue, Set.empty)
      val state   = State(tracers, costs, best)
      Iterator.iterate(state)(_.next).findMap(_.solution)


  val maze: Maze = Maze(lines.map(_.toVector).toVector)

  lazy val answer1: Long = maze.solve1
  lazy val answer2: Long = maze.solve2
