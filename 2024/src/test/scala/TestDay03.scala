import org.scalatest.funsuite.AnyFunSuite

class TestDay03 extends AnyFunSuite:
  test("Day03") {
    assertResult(179834255)(Day03.answer1)
    assertResult(80570939)(Day03.answer2)
  }
