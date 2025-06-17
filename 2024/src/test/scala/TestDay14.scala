import org.scalatest.funsuite.AnyFunSuite

class TestDay14 extends AnyFunSuite:
  test("Day14") {
    assertResult(233709840L)(actual = Day14.answer1)
    assertResult(6620L)(actual = Day14.answer2)
  }
