import org.scalatest.funsuite.AnyFunSuite

class TestSolutionsMarcella extends AnyFunSuite:

  test("Day03Marcella [43ms]") {
    assertResult(8139)(actual = Day03Marcella.answer1)
    assertResult(2668)(actual = Day03Marcella.answer2)
  }
  test("Day04Marcella [10ms]") {
    assertResult(651)(actual = Day04Marcella.answer1)
    assertResult(956)(actual = Day04Marcella.answer2)
  }