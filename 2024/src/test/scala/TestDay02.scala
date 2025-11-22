import org.scalatest.funsuite.AnyFunSuite

class TestDay02 extends AnyFunSuite:
  test("Day02") {
    assertResult(486)(Day02.answer1)
    assertResult(540)(Day02.answer2)
  }
