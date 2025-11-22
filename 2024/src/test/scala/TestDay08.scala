import org.scalatest.funsuite.AnyFunSuite

class TestDay08 extends AnyFunSuite:
  test("Day08") {
    assertResult(327)(Day08.answer1)
    assertResult(1233)(Day08.answer2)
  }
