import org.scalatest.funsuite.AnyFunSuite

class TestSolutions extends AnyFunSuite:

  test("Day01"):
    assertResult(477)(Day01.answer1)
    assertResult(390)(Day01.answer2)

  test("Day02"):
    assertResult(5368)(Day02.answer1)
    assertResult("cvgywxqubnuaefmsldjrpfzyi")(Day02.answer2)

  test("Day03"):
    assertResult(107043)(Day03.answer1)
    assertResult(346)(Day03.answer2)

  test("Day04"):
    assertResult(103720)(Day04.answer1)
    assertResult(110913)(Day04.answer2)

  test("Day05"):
    assertResult(11310)(Day05.answer1)
    assertResult(6020)(Day05.answer2)

  test("Day06"):
    assertResult(2906)(Day06.answer1)
    assertResult(50530)(Day06.answer2)

  test("Day07"):
    assertResult("FHICMRTXYDBOAJNPWQGVZUEKLS")(Day07.answer1)
    assertResult(946)(Day07.answer2)

  test("Day08"):
    assertResult(47244)(Day08.answer1)
    assertResult(17267)(Day08.answer2)

  test("Day09"):
    assertResult(412117)(Day09.answer1)
    assertResult(3444129546L)(Day09.answer2)

  test("Day10"):
    assertResult(
      """######..#####....####...#....#..#.........##.......###..#.....
        |#.......#....#..#....#..#....#..#........#..#.......#...#.....
        |#.......#....#..#........#..#...#.......#....#......#...#.....
        |#.......#....#..#........#..#...#.......#....#......#...#.....
        |#####...#####...#.........##....#.......#....#......#...#.....
        |#.......#..#....#.........##....#.......######......#...#.....
        |#.......#...#...#........#..#...#.......#....#......#...#.....
        |#.......#...#...#........#..#...#.......#....#..#...#...#.....
        |#.......#....#..#....#..#....#..#.......#....#..#...#...#.....
        |######..#....#...####...#....#..######..#....#...###....######
        |""".stripMargin)(Day10.sky.asString)
    assertResult(10813)(Day10.answer2)

  test("Day11"):
    assertResult("21,37")(Day11.answer1)
    assertResult("236,146,12")(Day11.answer2)

  test("Day12"):
    assertResult(2909)(Day12.answer1)
    assertResult(2500000001175L)(Day12.answer2)

  test("Day13"):
    assertResult("41,22")(Day13.answer1)
    assertResult("84,90")(Day13.answer2)

  test("Day14"):
    assertResult(1150511382)(Day14.answer1)
    assertResult(20173656)(Day14.answer2)

  test("Day15"):
    assertResult(250648)(Day15.answer1)
    assertResult(42224)(Day15.answer2)

  test("Day16"):
    assertResult(640)(Day16.answer1)
    assertResult(472)(Day16.answer2)

  test("Day17"):
    assertResult(34379)(Day17.answer1)
    assertResult(28015)(Day17.answer2)

  test("Day18"):
    assertResult(637550)(Day18.answer1)
    assertResult(201465)(Day18.answer2)

  test("Day19"):
    assertResult(978)(Day19.answer1)
    assertResult(10996992)(Day19.answer2)

  test("Day20"):
    assertResult(3721)(Day20.answer1)
    assertResult(8613)(Day20.answer2)

  test("Day21"):
    assertResult(1024276)(Day21.answer1)
    assertResult(5876609)(Day21.answer2)

  test("Day22"):
    assertResult(7402)(Day22.answer1)
    assertResult(1025)(Day22.answer2)

  test("Day23"):
    assertResult(309)(Day23.answer1)
    assertResult(119011326)(Day23.answer2)

  test("Day24"):
    assertResult(20340)(Day24.answer1)
    assertResult(3862)(Day24.answer2)

  test("Day25"):
    assertResult(324)(Day25.answer1)
