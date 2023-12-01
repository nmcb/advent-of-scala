import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [266ms]") {
    assertResult(54916)(actual = Day01.answer1)
    assertResult(54728)(actual = Day01.answer2)
  }
