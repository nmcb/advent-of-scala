import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay02 extends AnyFunSuite:
  test("Day02") {
    assertResult(486)(actual = Day02.answer1)
    assertResult(540)(actual = Day02.answer2)
  }
