import org.scalatest.funsuite.AnyFunSuite

class TestDay05 extends AnyFunSuite:
  test("Day06"):
    assertResult(601L)(Day05.answer1)
    assertResult(367899984917516L)(Day05.answer2)
  