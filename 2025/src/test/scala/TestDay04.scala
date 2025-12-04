import org.scalatest.funsuite.AnyFunSuite

class TestDay04 extends AnyFunSuite:
  test("Day04"):
    assertResult(1493)(Day04.answer1)
    assertResult(9194)(Day04.answer2)
  