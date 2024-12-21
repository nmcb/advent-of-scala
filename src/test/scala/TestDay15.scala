import org.scalatest.funsuite.AnyFunSuite

class TestDay15 extends AnyFunSuite:
  test("Day15") {
    assertResult(1552463)(actual = Day15.answer1)
    assertResult(1554058)(actual = Day15.answer2)
  }
