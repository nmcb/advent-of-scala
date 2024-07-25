import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(1069)(actual = Day01.answer1)
    assertResult(1268)(actual = Day01.answer2)
  }