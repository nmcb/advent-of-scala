import org.scalatest.funsuite.AnyFunSuite

class TestDay01 extends AnyFunSuite:
  test("Day01") {
    assertResult(2057374)(Day01.answer1)
    assertResult(23177084)(Day01.answer2)
  }
  