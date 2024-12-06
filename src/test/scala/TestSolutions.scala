import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:
  test("Day01") {
    assertResult( 2057374)(actual = Day01.answer1)
    assertResult(23177084)(actual = Day01.answer2)
  }
  test("Day02") {
    assertResult(486)(actual = Day02.answer1)
    assertResult(540)(actual = Day02.answer2)
  }
  test("Day03") {
    assertResult(179834255)(actual = Day03.answer1)
    assertResult( 80570939)(actual = Day03.answer2)
  }
  test("Day04") {
    assertResult(2618)(actual = Day04.answer1)
    assertResult(2011)(actual = Day04.answer2)
  }
  test("Day05") {
    assertResult(4766)(actual = Day05.answer1)
    assertResult(6257)(actual = Day05.answer2)
  }
  test("Day06") {
    assertResult(4663)(actual = Day06.answer1)
    assertResult(1530)(actual = Day06.answer2)
  }
