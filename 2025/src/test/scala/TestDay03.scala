import org.scalatest.funsuite.AnyFunSuite

class TestDay03 extends AnyFunSuite:
  test("Day03"):
    assertResult(17074L)(Day03.answer1)
    assertResult(169512729575727L)(Day03.answer2)
  