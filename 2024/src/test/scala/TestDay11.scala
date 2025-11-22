import org.scalatest.funsuite.AnyFunSuite

class TestDay11 extends AnyFunSuite:
  test("Day11") {
    assertResult(217443L)(Day11.answer1)
    assertResult(257246536026785L)(Day11.answer2)
  }
