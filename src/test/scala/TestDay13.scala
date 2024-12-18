import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay13 extends AnyFunSuite:
  test("Day13") {
    assertResult(1518548)(actual = Day12.answer1)
    assertResult( 909564)(actual = Day12.answer2)
  }
