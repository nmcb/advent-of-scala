import org.scalatest.funsuite.AnyFunSuite

class TestDay01 extends AnyFunSuite:
  test("Day01"):
    assertResult(1055)(Day01.answer1)
    assertResult(6386)(Day01.answer2)
  