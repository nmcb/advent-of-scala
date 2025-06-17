import org.scalatest.funsuite.AnyFunSuite

class TestDay05 extends AnyFunSuite:
  test("Day05") {
    assertResult(4766)(actual = Day05.answer1)
    assertResult(6257)(actual = Day05.answer2)
  }
  