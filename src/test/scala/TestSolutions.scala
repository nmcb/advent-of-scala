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
  test("Day04") {
    assertResult(337)(actual = Day04.answer1)
    assertResult(231)(actual = Day04.answer2)
  }
  test("Day05") {
    assertResult(372671)(actual = Day05.answer1)
    assertResult(25608480)(actual = Day05.answer2)
  }
  test("Day06") {
    assertResult(12841)(actual = Day06.answer1)
    assertResult(8038)(actual = Day06.answer2)
  }
  test("Day07") {
    assertResult("eqgvf")(actual = Day07.answer1)
    assertResult(757)(actual = Day07.answer2)
  }
  test("Day08") {
    assertResult(4416)(actual = Day08.answer1)
    assertResult(5199)(actual = Day08.answer2)
  }
  test("Day09") {
    assertResult(8337)(actual = Day09.answer1)
    assertResult(4330)(actual = Day09.answer2)
  }
