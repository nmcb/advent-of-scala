import org.scalatest.funsuite.AnyFunSuite

class TestDay21 extends AnyFunSuite:
  test("Day21") {
    assertResult(163920)(actual = Day21.answer1)
    assertResult(204040805018350L)(actual = Day21.answer2)
  }
