import org.scalatest.funsuite.AnyFunSuite

class TestDay23 extends AnyFunSuite:
  test("Day23") {
    assertResult(1304)(Day23.answer1)
    assertResult("ao,es,fe,if,in,io,ky,qq,rd,rn,rv,vc,vl")(Day23.answer2)
  }
