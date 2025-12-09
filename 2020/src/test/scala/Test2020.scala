import aoc2020.{Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13, Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25}
import org.scalatest.funsuite.AnyFunSuite

class Test2020 extends AnyFunSuite:

  test("Day 1: Report Repair"):
    assertResult(719796)(Day01.answer1)
    assertResult(144554112)(Day01.answer2)

  test("Day 2: Password Philosophy"):
    assertResult(528)(Day02.answer1)
    assertResult(497)(Day02.answer2)

  test("Day 3: Toboggan Trajectory"):
    assertResult(242)(Day03.answer1)
    assertResult(2265549792L)(Day03.answer2)

  test("Day 4: Passport Processing"):
    assertResult(245)(Day04.answer1)
    assertResult(133)(Day04.answer2)

  test("Day 5: Binary Boarding"):
    assertResult(806)(Day05.answer1)
    assertResult(562)(Day05.answer2)

  test("Day 6: Custom Customs"):
    assertResult(6680)(Day06.answer1)
    assertResult(3117)(Day06.answer2)

  test("Day 7: Handy Haversacks"):
    assertResult(208)(Day07.answer1)
    assertResult(1664)(Day07.answer2)

  test("Day 8: Handheld Halting"):
    assertResult(1818)(Day08.answer1)
    assertResult(631)(Day08.answer2)

  test("Day 9: Encoding Error"):
    assertResult(373803594L)(Day09.answer1)
    assertResult(51152360L)(Day09.answer2)

  test("Day 10: Adapter Array"):
    assertResult(1820)(Day10.answer1)
    assertResult(3454189699072L)(Day10.answer2)

  test("Day 11: Seating System"):
    assertResult(2183)(Day11.answer1)
    assertResult(1990)(Day11.answer2)

  test("Day 12: Rain Risk"):
    assertResult(882)(Day12.answer1)
    assertResult(28885)(Day12.answer2)

  test("Day 13: Shuttle Search"):
    assertResult(2545)(Day13.answer1)
    assertResult(266204454441577L)(Day13.answer2)

  test("Day 14: Docking Data"):
    assertResult(7997531787333L)(Day14.answer1)
    assertResult(3564822193820L)(Day14.answer2)

  test("Day 15: Rambunctious Recitation"):
    assertResult(376)(Day15.answer1)
    assertResult(323780)(Day15.answer2)

  test("Day 16: Ticket Translation"):
    assertResult(20091)(Day16.answer1)
    assertResult(2325343130651L)(Day16.answer2)

  test("Day 17: Conway Cubes"):
    assertResult(388)(Day17.answer1)
    assertResult(2280)(Day17.answer2)

  test("Day 18: Operation Order"):
    assertResult(909027960608791L)(Day18.answer1)
    assertResult(321176691637769L)(Day18.answer2)

  test("Day 19: Monster Messages"):
    assertResult(235)(Day19.answer1)
    assertResult(379)(Day19.answer2)

  test("Day 20: Jurassic Jigsaw"):
    assertResult(20913499394191L)(Day20.answer1)
    assertResult(2209)(Day20.answer2)

  test("Day 21: Allergen Assessment"):
    assertResult(2614)(Day21.answer1)
    assertResult("qhvz,kbcpn,fzsl,mjzrj,bmj,mksmf,gptv,kgkrhg")(Day21.answer2)

  test("Day 22: Crab Combat"):
    assertResult(33393)(Day22.answer1)
    assertResult(31963)(Day22.answer2)

  test("Day 23: Crab Cups"):
    assertResult(25468379)(Day23.answer1)
    assertResult(474747880250L)(Day23.answer2)

  test("Day 24: Lobby Layout"):
    assertResult(497)(Day24.answer1)
    assertResult(4156)(Day24.answer2)

  test("Day 25: Combo Breaker"):
    assertResult(6198540)(Day25.answer1)
