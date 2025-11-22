import org.scalatest.funsuite.AnyFunSuite

class TestDay22 extends AnyFunSuite:
  test("Day22") {
    assertResult(14476723788L)(Day22.answer1)
    assertResult(1630)(Day22.answer2)
  }
