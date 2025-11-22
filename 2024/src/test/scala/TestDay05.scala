import org.scalatest.funsuite.AnyFunSuite

class TestDay05 extends AnyFunSuite:
  test("Day05") {
    assertResult(4766)(Day05.answer1)
    assertResult(6257)(Day05.answer2)
  }
  