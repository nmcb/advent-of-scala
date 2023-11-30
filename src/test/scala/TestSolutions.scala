import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [32ms]") {
    assertResult(69501)(actual = Day01.answer1)
    assertResult(202346)(actual = Day01.answer2)
  }
