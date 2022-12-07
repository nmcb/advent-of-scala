import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [32ms]") {
    assertResult( 69501)(actual = Day01.answer1)
    assertResult(202346)(actual = Day01.answer2)
  }
  test("Day02 [11ms]") {
    assertResult(11449)(actual = Day02.answer1)
    assertResult(13187)(actual = Day02.answer2)
  }
  test("Day03 [43ms]") {
    assertResult(8139)(actual = Day03.answer1)
    assertResult(2668)(actual = Day03.answer2)
  }
  test("Day04 [10ms]") {
    assertResult(651)(actual = Day04.answer1)
    assertResult(956)(actual = Day04.answer2)
  }
  test("Day05 [8ms]") {
    assertResult("WHTLRMZRC")(actual = Day05.answer1)
    assertResult("GMPMLWNMG")(actual = Day05.answer2)
  }
  test("Day06 [5ms]") {
    assertResult(1093)(actual = Day06.answer1)
    assertResult(3534)(actual = Day06.answer2)
  }
  test("Day07 [3ms]") {
    assertResult(1915606)(actual = Day07.answer1)
    assertResult(5025657)(actual = Day07.answer2)
  }
