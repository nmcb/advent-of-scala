package marcella

import org.scalatest.funsuite.AnyFunSuite

class TestSolutionsMarcella extends AnyFunSuite:

  test("Day02") {
    assertResult(11449)(actual = Day02Marcella.answer1)
    assertResult(13187)(actual = Day02Marcella.answer2)
  }
  test("Day03") {
    assertResult(8139)(actual = Day03Marcella.answer1)
    assertResult(2668)(actual = Day03Marcella.answer2)
  }
  test("Day04") {
    assertResult(651)(actual = Day04Marcella.answer1)
    assertResult(956)(actual = Day04Marcella.answer2)
  }
  test("Day05") {
    assertResult("SBSVQBGLM")(actual = Day05Marcella.answer1)
    assertResult("SLDRWVVNH")(actual = Day05Marcella.answer2)
  }
  test("Day06") {
    assertResult(1093)(actual = Day06Marcella.answer1)
    assertResult(3534)(actual = Day06Marcella.answer2)
  }
