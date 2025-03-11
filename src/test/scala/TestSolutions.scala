import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(1069)(actual = Day01.answer1)
    assertResult(1268)(actual = Day01.answer2)
  }
  test("Day02") {
    assertResult(45158)(actual = Day02.answer1)
    assertResult(294)(actual = Day02.answer2)
  }
  test("Day03") {
    assertResult(475)(actual = Day03.answer1)
    assertResult(279138)(actual = Day03.answer2)
  }