import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay09 extends AnyFunSuite:
  test("Day09") {
    assertResult(6259790630969L)(actual = Day09.answer1)
    assertResult(6289564433984L)(actual = Day09.answer2)
    assertResult(6289564433984L)(actual = Day09.answer2Mutable)
  }
