package aoc2025

import nmcb.*

object Day08 extends AoC:

  type Box  = (x: Int, y: Int, z: Int)

  extension (box: Box)

    infix def distance(that: Box): Double =
      val xx = (box.x - that.x).toDouble * (box.x - that.x)
      val yy = (box.y - that.y).toDouble * (box.y - that.y)
      val zz = (box.z - that.z).toDouble * (box.z - that.z)
      math.sqrt(xx + yy + zz)

  type Pair      = (a: Box, b: Box)
  type CircuitId = Long
  type State     = (ids: Map[Box, CircuitId], circuits: Map[CircuitId, Set[Box]], pair: Pair, id: CircuitId)

  object State:
    def empty: State = (
      ids      = Map.empty[Box, CircuitId],
      circuits = Map.empty[CircuitId, Set[Box]],
      pair     = (x = 0, y = 0, z = 0) -> (0, 0, 0),
      id       = 0L
    )

  def solve(boxes: Vector[Box]): Iterator[State] =

    val sortedPairIterator =
      boxes
        .tails
        .toVector
        .tail
        .flatMap(boxes.zip)
        .sortBy((a,b) => a.distance(b))
        .iterator

    sortedPairIterator
      .scanLeft(State.empty):
        case ((ids, circuits, _, id), (a, b)) =>
          (ids.get(a), ids.get(b)) match
            case (None, None) =>
              (ids + (a -> id) + (b -> id), circuits + (id -> Set(a,b)), a -> b, id + 1)
            case (Some(c), None) =>
              (ids + (b -> c), circuits + (c -> (circuits(c) + b)), a -> b, id)
            case (None, Some(d)) =>
              (ids + (a -> d), circuits + (d -> (circuits(d) + a)), a -> b, id)
            case (Some(c), Some(d)) =>
              (ids ++ circuits(d).map(_ -> c), circuits - d + (c -> (circuits(c) ++ circuits(d))), a -> b, id)

  def solve1(boxes: Vector[Box], count: Int): Long =
    val found = solve(boxes).drop(count).next()
    found.circuits.values.map(_.size).toVector.sorted.takeRight(3).product

  def solve2(boxes: Vector[Box]): Long =
    val found = solve(boxes).dropWhile(state => state.ids.size != boxes.size || state.circuits.size != 1).next()
    found.pair.a.x.toLong * found.pair.b.x

  val boxes: Vector[Box] = lines.collect:
    case s"$x,$y,$z" => (x = x.toInt, y = y.toInt, z = z.toInt)

  lazy val answer1: Long = solve1(boxes, 1000)
  lazy val answer2: Long = solve2(boxes)
