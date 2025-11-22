import org.scalatest.funsuite.AnyFunSuite

class TestDay15 extends AnyFunSuite:
  test("Day15") {
    assertResult(1552463)(Day15.answer1)
    assertResult(1554058)(Day15.answer2)
  }
