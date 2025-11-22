import org.scalatest.funsuite.AnyFunSuite

class TestDay06 extends AnyFunSuite:
  test("Day06") {
    assertResult(4663)(Day06.answer1)
    assertResult(1530)(Day06.answer2)
  }
