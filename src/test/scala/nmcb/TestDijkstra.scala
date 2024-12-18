package nmcb

import org.scalatest.funsuite.AnyFunSuite

import Dijkstra.*

class TestDijkstra extends AnyFunSuite:

  test("Dijkstra") {
    val input =
      """...#...
        |..#..#.
        |....#..
        |...#..#
        |..#..#.
        |.#..#..
        |#.#....
      """.stripMargin

    val grid   = Grid.fromString(input)
    val graph  = Graph.fromGrid(grid, '.')
    val result = Dijkstra.run(graph, grid.minPos)
    val output = result.pathTo(grid.maxPos).toTrail.foldLeft(grid)(_.updated(_, 'O')).asString

    assertResult(
      """OO.#OOO
        |.O#OO#O
        |.OOO#OO
        |...#OO#
        |..#OO#.
        |.#.O#..
        |#.#OOOO
        |""".stripMargin.trim)(actual = output)
  }