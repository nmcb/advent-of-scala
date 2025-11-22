import org.scalatest.funsuite.AnyFunSuite

class TestDay09 extends AnyFunSuite:
  test("Day09") {
    assertResult(6259790630969L)(Day09.answer1)
    assertResult(6289564433984L)(Day09.answer2)
    assertResult(6289564433984L)(Day09.answer2Mutable)
  }
