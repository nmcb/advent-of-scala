package nmcb

import predef.*

import Pos.*

import org.scalatest.funsuite.AnyFunSuite

import Dijkstra.*

val input =
  """.###...
    |.##..#.
    |....#..
    |...#..#
    |..#..#.
    |..#.#..
    |#.#....
    """.stripMargin

val shortest =
  """O###OOO
    |O##OO#O
    |OOOO#OO
    |...#OO#
    |..#OO#.
    |..#O#..
    |#.#OOOO
    |""".stripMargin.trim

class TestDijkstra extends AnyFunSuite:

  test("Dijkstra.run") {
    val grid   = Grid.fromString(input)
    val graph  = Graph.fromGrid(grid, '.')
    val result = Dijkstra.run[Pos](graph, grid.minPos)
    val output = result.pathTo(grid.maxPos).toTrail.foldLeft(grid)(_.updated(_, 'O')).asString

    assertResult(expected = shortest)(actual = output)
  }

  test("Dijkstra.reachable") {
    val grid      = Grid.fromString(input)
    val cluster   = grid.findAll('.')
    val reachable = Dijkstra.reachable[Pos](grid.minPos, _.adjWithinGrid(grid, _.element == '.'))

    assertResult(expected = cluster)(actual = reachable)
  }