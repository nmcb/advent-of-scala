import org.scalatest.funsuite.AnyFunSuite

class TestDay07 extends AnyFunSuite:
  test("Day07") {
    assertResult(21572148763543L)(Day07.answer1)
    assertResult(581941094529163L)(Day07.answer2)
    assertResult(581941094529163L)(Day07.answer2Stewart)
  }
