package aoc2025

import nmcb.*
import nmcb.predef.*

object Day08 extends AoC:

  type Box  = (x: Int, y: Int, z: Int)

  extension (pair: (Box, Box))

    def distance: Double =
      val xx = (pair.left.x - pair.right.x).toDouble * (pair.left.x - pair.right.x)
      val yy = (pair.left.y - pair.right.y).toDouble * (pair.left.y - pair.right.y)
      val zz = (pair.left.z - pair.right.z).toDouble * (pair.left.z - pair.right.z)
      math.sqrt(xx + yy + zz)

  type CircuitId = Long
  type State     = (ids: Map[Box, CircuitId], circuits: Map[CircuitId, Set[Box]], pair: (Box, Box), id: CircuitId)

  object State:
    def empty: State = (
      ids      = Map.empty[Box, CircuitId],
      circuits = Map.empty[CircuitId, Set[Box]],
      pair     = (0, 0, 0) -> (0, 0, 0),
      id       = 0L
    )

  def solve(boxes: Vector[Box]): Iterator[State] =
    boxes
      .pairs(using Ordering.by(_.distance))
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
    val found = solve(boxes).nth(count)
    found.circuits.values.map(_.size).toVector.sorted.takeRight(3).product

  def solve2(boxes: Vector[Box]): Long =
    val found = solve(boxes).findFirst(state => state.ids.size == boxes.size && state.circuits.size == 1)
    found.pair.left.x.toLong * found.pair.right.x

  val boxes: Vector[Box] = lines.collect:
    case s"$x,$y,$z" => (x = x.toInt, y = y.toInt, z = z.toInt)

  lazy val answer1: Long = solve1(boxes, 1000)
  lazy val answer2: Long = solve2(boxes)
