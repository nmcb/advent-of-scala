import org.scalatest.funsuite.AnyFunSuite

class TestDay21 extends AnyFunSuite:
  test("Day21") {
    assertResult(163920)(Day21.answer1)
    assertResult(204040805018350L)(Day21.answer2)
  }
