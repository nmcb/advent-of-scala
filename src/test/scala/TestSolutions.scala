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
  test("Day03") {
    assertResult(498559)(actual = Day03.answer1)
    assertResult(72246648)(actual = Day03.answer2)
  }
  test("Day04") {
    assertResult(24175)(actual = Day04.answer1)
    assertResult(18846301)(actual = Day04.answer2)
  }
  test("Day05") {
    assertResult(51580674)(actual = Day05.answer1)
    assertResult(99751240)(actual = Day05.answer2)
  }
  test("Day06") {
    assertResult(1413720)(actual = Day06.answer1)
    assertResult(30565288)(actual = Day06.answer2)
  }
  test("Day07") {
    assertResult(250347426)(actual = Day07.answer1)
    assertResult(251224870)(actual = Day07.answer2)
  }
  test("Day08") {
    assertResult(18673)(actual = Day08.answer1)
    assertResult(17972669116327L)(actual = Day08.answer2)
  }
  test("Day09") {
    assertResult(1798691765)(actual = Day09.answer1)
    assertResult(1104)(actual = Day09.answer2)
  }
  test("Day10") {
    assertResult(6733)(actual = Day10.answer1)
    assertResult(435)(actual = Day10.answer2)
  }
  test("Day11") {
    assertResult(9947476)(actual = Day11.answer1)
    assertResult(519939907614L)(actual = Day11.answer2)
  }
  test("Day12") {
    assertResult(7506)(actual = Day12.answer1)
    assertResult(548241300348335L)(actual = Day12.answer2)
  }
