import org.scalatest.funsuite.AnyFunSuite

class TestDay19 extends AnyFunSuite:
  test("Day19") {
    assertResult(353)(actual = Day19.answer1)
    assertResult(880877787214477L)(actual = Day19.answer2)
  }
