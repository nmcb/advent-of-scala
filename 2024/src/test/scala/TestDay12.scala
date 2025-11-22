import org.scalatest.funsuite.AnyFunSuite

class TestDay12 extends AnyFunSuite:
  test("Day12") {
    assertResult(1518548)(Day12.answer1)
    assertResult( 909564)(Day12.answer2)
  }
