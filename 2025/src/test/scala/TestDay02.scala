import org.scalatest.funsuite.AnyFunSuite

class TestDay02 extends AnyFunSuite:
  test("Day02"):
    assertResult(23701357374L)(Day02.answer1)
    assertResult(34284458938L)(Day02.answer2)
  