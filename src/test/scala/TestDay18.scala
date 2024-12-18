import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay18 extends AnyFunSuite:
  test("Day18") {
    assertResult(436)(actual = Day18.answer1)
    assertResult(Pos(61,50))(actual = Day18.answer2)
  }
