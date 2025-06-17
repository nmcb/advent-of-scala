import org.scalatest.funsuite.AnyFunSuite

class TestParser extends AnyFunSuite:

  test("Day12 - Json.parse") {
    import Day12.*
    assertResult(Num(12))(actual = Json.parse("12"))
    assertResult(Num(-1))(actual = Json.parse("-1"))
    assertResult(Str(""))(actual = Json.parse("\"\""))
    assertResult(Str("a"))(actual = Json.parse("\"a\""))
    assertResult(Str("ab"))(actual = Json.parse("\"ab\""))
    assertResult(Arr(List(Num(1),Obj(Map("a" -> Num(1),"b" -> Num(2))))))(actual = Json.parse("""[1,{"a":1,"b":2}]"""))
    assertResult(Obj(Map("a" -> Num(1),"b" -> Arr(List(Num(1),Num(2))))))(actual = Json.parse("""{"a":1,"b":[1,2]}"""))
  }
