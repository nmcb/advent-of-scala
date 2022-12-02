import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [1ms]") {
    assertResult( 69501)(actual = Day01.answer1)
    assertResult(202346)(actual = Day01.answer2)
  }
  test("Day02 [1ms]") {
    assertResult(11449)(actual = Day02.answer1)
    assertResult(13187)(actual = Day02.answer2)
  }
