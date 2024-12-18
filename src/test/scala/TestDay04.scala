import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay04 extends AnyFunSuite:
  test("Day04") {
    assertResult(2618)(actual = Day04.answer1)
    assertResult(2011)(actual = Day04.answer2)
  }

