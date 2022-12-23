import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01 [32ms]") {
    assertResult(69501)(actual = Day01.answer1)
    assertResult(202346)(actual = Day01.answer2)
  }
  test("Day02 [11ms]") {
    assertResult(11449)(actual = Day02.answer1)
    assertResult(13187)(actual = Day02.answer2)
  }
  test("Day03 [43ms]") {
    assertResult(8139)(actual = Day03.answer1)
    assertResult(2668)(actual = Day03.answer2)
  }
  test("Day04 [10ms]") {
    assertResult(651)(actual = Day04.answer1)
    assertResult(956)(actual = Day04.answer2)
  }
  test("Day05 [8ms]") {
    assertResult("WHTLRMZRC")(actual = Day05.answer1)
    assertResult("GMPMLWNMG")(actual = Day05.answer2)
  }
  test("Day06 [5ms]") {
    assertResult(1093)(actual = Day06.answer1)
    assertResult(3534)(actual = Day06.answer2)
  }
  test("Day07 [3ms]") {
    assertResult(1915606)(actual = Day07.answer1)
    assertResult(5025657)(actual = Day07.answer2)
  }
  test("Day08 [745ms]") {
    assertResult(1818)(actual = Day08.answer1)
    assertResult(368368)(actual = Day08.answer2)
  }
  test("Day09 [311ms]") {
    assertResult(6376)(actual = Day09.answer1)
    assertResult(2607)(actual = Day09.answer2)
  }
  test("Day10 [4ms]") {
    assertResult(12520)(actual = Day10.answer1)
    assertResult(
      """####.#..#.###..####.###....##..##..#....
        |#....#..#.#..#....#.#..#....#.#..#.#....
        |###..####.#..#...#..#..#....#.#....#....
        |#....#..#.###...#...###.....#.#.##.#....
        |#....#..#.#....#....#....#..#.#..#.#....
        |####.#..#.#....####.#.....##...###.####.
        |""".stripMargin.trim)(actual = Day10.answer2)
  }
  test("Day11 [131ms]") {
    assertResult(98280)(actual = Day11.answer1)
    assertResult(17673687232L)(actual = Day11.answer2)
  }
  test("Day12 [15s]") {
    assertResult(383)(actual = Day12.answer1)
    assertResult(377)(actual = Day12.answer2)
  }
  test("Day13 [7ms]") {
    assertResult(5760)(actual = Day13.answer1)
    assertResult(26670)(actual = Day13.answer2)
  }
  ignore("Day14 [73s]") {
    assertResult(793)(actual = Day14.answer1)
    assertResult(24166)(actual = Day14.answer2)
  }
  test("Day15 [6ms]") {
    assertResult(5870801)(actual = Day15.answer1)
    assertResult(10908230916597L)(actual = Day15.answer2)
  }
  test("Day16") {
    assertResult(2265)(actual = Day16.answer1)
    assertResult(2811)(actual = Day16.answer2)
  }
  test("Day17") {
    assertResult(3130)(actual = Day17.answer1)
//  assertResult(???)(actual = Day17.answer2) // FUCK FUCK FUCK !!!
  }
  test("Day18") {
    assertResult(4340)(actual = Day18.answer1)
    assertResult(2468)(actual = Day18.answer2)
  }
  test("Day19") {
    assertResult(1177)(actual = Day19.answer1)
    assertResult(62744)(actual = Day19.answer2)
  }
  test("Day20 [2413]") {
    assertResult(8302L)(actual = Day20.answer1)
    assertResult(656575624777L)(actual = Day20.answer2)
  }
  test("Day21 [80ms]") {
    assertResult(282285213953670L)(actual = Day21.answer1)
    assertResult(3699945358564L)(actual = Day21.answer2)
  }
  test("Day22 [60ms]") {
    assertResult(56372)(actual = Day22.answer1)
    assertResult(197047)(actual = Day22.answer2)
  }
  ignore("Day23 [385s]") {
    assertResult(4109)(actual = Day23.answer1)
    assertResult(197047)(actual = Day23.answer2)
  }
