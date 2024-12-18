import org.scalatest.funsuite.AnyFunSuite

class TestDay10 extends AnyFunSuite:
  test("Day10") {
    assertResult(550)(actual = Day10.answer1)
    assertResult(1255)(actual = Day10.answer2)
  }
