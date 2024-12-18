import org.scalatest.funsuite.AnyFunSuite
import nmcb.*

class TestDay17 extends AnyFunSuite:
  test("Day17") {
    assertResult("1,7,2,1,4,1,5,4,0")(actual = Day17.answer1)
    assertResult(37221261688308L)(actual = Day17.answer2)
  }
