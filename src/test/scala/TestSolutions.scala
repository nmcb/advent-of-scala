import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(666)(actual = Day01.answer1)
    assertResult(666)(actual = Day01.answer2)
  }