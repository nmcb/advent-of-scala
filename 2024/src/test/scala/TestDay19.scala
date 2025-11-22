import org.scalatest.funsuite.AnyFunSuite

class TestDay19 extends AnyFunSuite:
  test("Day19") {
    assertResult(353)(Day19.answer1)
    assertResult(880877787214477L)(Day19.answer2)
  }
