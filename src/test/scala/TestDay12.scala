import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay12 extends AnyFunSuite:
  test("Day12") {
    assertResult(1518548)(actual = Day12.answer1)
    assertResult( 909564)(actual = Day12.answer2)
  }
