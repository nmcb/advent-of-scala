import org.scalatest.funsuite.AnyFunSuite

class TestDay20 extends AnyFunSuite:
  test("Day20") {
    assertResult(1327)(Day20.answer1)
    assertResult(985737)(Day20.answer2)
  }
