import aoc2022.{Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25, Day26}
import org.scalatest.funsuite.AnyFunSuite

class Test2022 extends AnyFunSuite:

  test("Day 1: Calorie Counting"):
    assertResult(69501)(Day01.answer1)
    assertResult(202346)(Day01.answer2)

  test("Day 2: Rock Paper Scissors"):
    assertResult(11449)(Day02.answer1)
    assertResult(13187)(Day02.answer2)

  test("Day 3: Rucksack Reorganization"):
    assertResult(8139)(Day03.answer1)
    assertResult(2668)(Day03.answer2)

  test("Day 4: Camp Cleanup"):
    assertResult(651)(Day04.answer1)
    assertResult(956)(Day04.answer2)

  test("Day 5: Supply Stacks"):
    assertResult("WHTLRMZRC")(Day05.answer1)
    assertResult("GMPMLWNMG")(Day05.answer2)

  test("Day 6: Tuning Trouble"):
    assertResult(1093)(Day06.answer1)
    assertResult(3534)(Day06.answer2)

  test("Day 7: No Space Left On Device"):
    assertResult(1915606)(Day07.answer1)
    assertResult(5025657)(Day07.answer2)

  test("Day 8: Treetop Tree House"):
    assertResult(1818)(Day08.answer1)
    assertResult(368368)(Day08.answer2)

  test("Day 9: Rope Bridge"):
    assertResult(6376)(Day09.answer1)
    assertResult(2607)(Day09.answer2)

  test("Day 10: Cathode-Ray Tube"):
    assertResult(12520)(Day10.answer1)
    assertResult(
      """####.#..#.###..####.###....##..##..#....
        |#....#..#.#..#....#.#..#....#.#..#.#....
        |###..####.#..#...#..#..#....#.#....#....
        |#....#..#.###...#...###.....#.#.##.#....
        |#....#..#.#....#....#....#..#.#..#.#....
        |####.#..#.#....####.#.....##...###.####.
        |""".stripMargin.trim)(Day10.answer2)

  test("Day 11: Monkey in the Middle"):
    assertResult(98280)(Day11.answer1)
    assertResult(17673687232L)(Day11.answer2)

  test("Day 12: Hill Climbing Algorithm"):
    assertResult(383)(Day12.answer1)
    assertResult(377)(Day12.answer2)

  test("Day 13: Distress Signal"):
    assertResult(5760)(Day13.answer1)
    assertResult(26670)(Day13.answer2)

  test("Day 14: Regolith Reservoir"):
    assertResult(793)(Day14.answer1)
    assertResult(24166)(Day14.answer2)

  test("Day 15: Beacon Exclusion Zone"):
    assertResult(5870801)(Day15.answer1)
    assertResult(10908230916597L)(Day15.answer2)

  test("Day 16: Proboscidea Volcanium"):
    assertResult(2265)(Day16.answer1)
    assertResult(2811)(Day16.answer2)

  test("Day 17: Pyroclastic Flow"):
    assertResult(3130 )(Day17.answer1)
    assertResult(1556521739139L)(Day17.answer2)

  test("Day 18: Boiling Boulders"):
    assertResult(4340)(Day18.answer1)
    assertResult(2468)(Day18.answer2)

  test("Day 19: Not Enough Minerals"):
    assertResult(1177)(Day19.answer1)
    assertResult(62744)(Day19.answer2)

  test("Day 20: Grove Positioning System"):
    assertResult(8302L)(Day20.answer1)
    assertResult(656575624777L)(Day20.answer2)

  test("Day 21: Monkey Math"):
    assertResult(282285213953670L)(Day21.answer1)
    assertResult(3699945358564L)(Day21.answer2)

  test("Day 22: Monkey Map"):
    assertResult(56372)(Day22.answer1)
    assertResult(197047)(Day22.answer2)

  test("Day 23: Unstable Diffusion"):
    assertResult(4109)(Day23.answer1)
    assertResult(1055)(Day23.answer2)

  test("Day 24: Blizzard Basin"):
    assertResult(292)(Day24.answer1)
    assertResult(816)(Day24.answer2)

  test("Day 25: Full of Hot Air"):
    assertResult("2=112--220-=-00=-=20")(Day25.answer1)

  test("Day 26: Look and Say"):
    assertResult(20)(Day26.answer1)
