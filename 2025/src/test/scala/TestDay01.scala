import org.scalatest.funsuite.AnyFunSuite

class TestDay01 extends AnyFunSuite:
  test("Day01") {
    assertResult(1)(Day01.answer1)
    assertResult(5)(Day01.answer2)
  }
  