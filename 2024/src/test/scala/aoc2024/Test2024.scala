package aoc2024

import nmcb.pos.*

import org.scalatest.funsuite.AnyFunSuite

class Test2024 extends AnyFunSuite:
  test("Day 1: Historian Hysteria"):
    assertResult(2057374)(Day01.answer1)
    assertResult(23177084)(Day01.answer2)
  test("Day 2: Red-Nosed Reports"):
    assertResult(486)(Day02.answer1)
    assertResult(540)(Day02.answer2)
  test("Day 3: Mull It Over"):
    assertResult(179834255)(Day03.answer1)
    assertResult(80570939)(Day03.answer2)
  test("Day 4: Ceres Search"):
    assertResult(2618)(Day04.answer1)
    assertResult(2011)(Day04.answer2)
  test("Day 5: Print Queue"):
    assertResult(4766)(Day05.answer1)
    assertResult(6257)(Day05.answer2)
  test("Day 6: Guard Gallivant"):
    assertResult(4663)(Day06.answer1)
    assertResult(1530)(Day06.answer2)
  test("Day 7: Bridge Repair"):
    assertResult(21572148763543L)(Day07.answer1)
    assertResult(581941094529163L)(Day07.answer2)
    assertResult(581941094529163L)(Day07.answer2Stewart)
  test("Day 8: Resonant Collinearity"):
    assertResult(327)(Day08.answer1)
    assertResult(1233)(Day08.answer2)
  test("Day 9: Disk Fragmenter"):
    assertResult(6259790630969L)(Day09.answer1)
    assertResult(6289564433984L)(Day09.answer2)
    assertResult(6289564433984L)(Day09.answer2Mutable)
  test("Day 10: Hoof It"):
    assertResult(550)(Day10.answer1)
    assertResult(1255)(Day10.answer2)
  test("Day 11: Plutonian Pebbles"):
    assertResult(217443L)(Day11.answer1)
    assertResult(257246536026785L)(Day11.answer2)
  test("Day 12: Garden Groups"):
    assertResult(1518548)(Day12.answer1)
    assertResult(909564)(Day12.answer2)
  test("Day 13: Claw Contraption"):
    assertResult(1518548)(Day12.answer1)
    assertResult(909564)(Day12.answer2)
  test("Day 14: Restroom Redoubt"):
    assertResult(233709840L)(Day14.answer1)
    assertResult(6620L)(Day14.answer2)
  test("Day 15: Warehouse Woes"):
    assertResult(1552463)(Day15.answer1)
    assertResult(1554058)(Day15.answer2)
  test("Day 16: Reindeer Maze"):
    assertResult(66404)(Day16.answer1)
    assertResult(433)(Day16.answer2)
  test("Day 17: Chronospatial Computer"):
    assertResult("1,7,2,1,4,1,5,4,0")(Day17.answer1)
    assertResult(37221261688308L)(Day17.answer2)
  test("Day 18: RAM Run"):
    assertResult(436)(Day18.answer1)
    assertResult(Pos(61, 50))(Day18.answer2)
  test("Day 19: Linen Layout"):
    assertResult(353)(Day19.answer1)
    assertResult(880877787214477L)(Day19.answer2)
  test("Day 20: Race Condition"):
    assertResult(1327)(Day20.answer1)
    assertResult(985737)(Day20.answer2)
  test("Day 21: Keypad Conundrum"):
    assertResult(163920)(Day21.answer1)
    assertResult(204040805018350L)(Day21.answer2)
  test("Day 22: Monkey Market"):
    assertResult(14476723788L)(Day22.answer1)
    assertResult(1630)(Day22.answer2)
  test("Day 23: LAN Party"):
    assertResult(1304)(Day23.answer1)
    assertResult("ao,es,fe,if,in,io,ky,qq,rd,rn,rv,vc,vl")(Day23.answer2)
  test("Day 24: Crossed Wires"):
    assertResult(53755311654662L)(Day24.answer1)
    assertResult("dkr,ggk,hhh,htp,rhv,z05,z15,z20")(Day24.answer2)
  test("Day 25: Code Chronicle"):
    assertResult(3196)(Day25.answer1)