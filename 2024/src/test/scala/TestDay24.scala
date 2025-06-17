import org.scalatest.funsuite.AnyFunSuite

class TestDay24 extends AnyFunSuite:
  test("Day24") {
    assertResult(53755311654662L)(actual = Day24.answer1)
    assertResult("dkr,ggk,hhh,htp,rhv,z05,z15,z20")(actual = Day24.answer2)
  }
