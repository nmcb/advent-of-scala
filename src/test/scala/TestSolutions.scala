import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01") {
    assertResult(54916)(actual = Day01.answer1)
    assertResult(54728)(actual = Day01.answer2)
  }
  test("Day02") {
    assertResult(2600)(actual = Day02.answer1)
    assertResult(86036)(actual = Day02.answer2)
  }
