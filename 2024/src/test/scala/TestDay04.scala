import org.scalatest.funsuite.AnyFunSuite

class TestDay04 extends AnyFunSuite:
  test("Day04") {
    assertResult(2618)(Day04.answer1)
    assertResult(2011)(Day04.answer2)
  }
