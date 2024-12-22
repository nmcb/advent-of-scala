import org.scalatest.funsuite.AnyFunSuite

class TestDay22 extends AnyFunSuite:
  test("Day22") {
    assertResult(/*14476723788L*/ 37990510)(actual = Day22.answer1)
    assertResult(666)(actual = Day22.answer2)
  }
