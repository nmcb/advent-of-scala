import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:
  test("Day01") {
    assertResult( 2057374)(actual = Day01.answer1)
    assertResult(23177084)(actual = Day01.answer2)
                 23085022
  }
  test("Day02") {
    assertResult(486)(actual = Day02.answer1)
    assertResult(540)(actual = Day02.answer2)
  }
  test("Day03") {
    assertResult(179834255)(actual = Day03.answer1)
    assertResult( 80570939)(actual = Day03.answer2)
  }
  test("Day04") {
    assertResult(2618)(actual = Day04.answer1)
    assertResult(2011)(actual = Day04.answer2)
  }
  test("Day05") {
    assertResult(4766)(actual = Day05.answer1)
    assertResult(6257)(actual = Day05.answer2)
  }
  test("Day06") {
    assertResult(4663)(actual = Day06.answer1)
    assertResult(1530)(actual = Day06.answer2)
  }
  test("Day07") {
    assertResult( 21572148763543L)(actual = Day07.answer1)
    assertResult(581941094529163L)(actual = Day07.answer2)
    assertResult(581941094529163L)(actual = Day07.answer2Stewart)
  }
  test("Day08") {
    assertResult( 327)(actual = Day08.answer1)
    assertResult(1233)(actual = Day08.answer2)
  }
  test("Day09") {
    assertResult(6259790630969L)(actual = Day09.answer1)
    assertResult(6289564433984L)(actual = Day09.answer2)
    assertResult(6289564433984L)(actual = Day09.answer2Mutable)
  }
  test("Day10") {
    assertResult( 550)(actual = Day10.answer1)
    assertResult(1255)(actual = Day10.answer2)
  }
  test("Day11") {
    assertResult(         217443L)(actual = Day11.answer1)
    assertResult(257246536026785L)(actual = Day11.answer2)
  }
  test("Day12") {
    assertResult(1518548)(actual = Day12.answer1)
    assertResult( 909564)(actual = Day12.answer2)
  }
