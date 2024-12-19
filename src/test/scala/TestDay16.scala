import org.scalatest.funsuite.AnyFunSuite

class TestDay16 extends AnyFunSuite:
  test("Day16") {
    assertResult(66404)(actual = Day16.answer1)
    assertResult(433)(actual = Day16.answer2)
  }
