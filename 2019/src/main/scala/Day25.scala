import scala.io.Source

/** @see Credits - https://github.com/sim642 */
object Day25 extends App:

  import cpu.*

  def day: String  = getClass.getSimpleName.filter(_.isDigit).mkString
  def program: Mem = Mem.parse(Source.fromResource(s"input$day.txt").mkString.trim)

  def runInteractive(program: Mem): Unit =

    val reader: java.io.BufferedReader =
      Console.in

    val inputs: LazyList[Value] =
      LazyList.unfold(()): _ =>
        val c = reader.read()
        if c < 0 then None else Some((c.toLong, ()))

    val outputs = CPU(program, stdin = inputs).outputs
    outputs.foreach(c => print(c.toChar))

  private val Room =
    """== (.+) ==
      |.*
      |
      |Doors here lead:
      |((?:- \w+
      |)+)(?:
      |Items here:
      |((?:- [\w ]+
      |)+))?
      |(?:Command\?|(?:.|\n)*?(\d+))""".stripMargin.r.unanchored

  private val ListItem = """- ([\w ]+)""".r
  private val Take     = """You take the ([\w ]+)\.""".r.unanchored
  private val Drop     = """You drop the ([\w ]+)\.""".r.unanchored

  case class Droid(room: String, doors: Set[String], items: Set[String], inventory: Set[String], password: Option[Int])(val cpu: CPU):

    def run(command: String): Droid =
      val inputs    = (command + "\n").map(_.toLong).to(LazyList)
      val outputs   = cpu.copy(stdin = inputs).outputStates
      val stdout    = outputs.map((_,value) => value.toChar).mkString
      val (next, _) = outputs.last

      stdout match
        case Room(room, doors, items, pwd) =>
          val nextDoors = ListItem.findAllMatchIn(doors).map(_.group(1)).toSet
          val nextItems = if items == null then Set.empty[String] else ListItem.findAllMatchIn(items).map(_.group(1)).toSet
          Droid(room, nextDoors, nextItems, inventory, password.orElse(Option(pwd).map(_.toInt)))(next)
        case Take(item) =>
          Droid(room, doors, items - item, inventory + item, password)(next)
        case Drop(item) =>
          Droid(room, doors, items + item, inventory - item, password)(next)

    def move(door: String): Droid =
      require(doors.contains(door))
      run(door)

    infix def take(item: String): Droid =
      require(items.contains(item))
      run(s"take $item")

    infix def drop(item: String): Droid =
      require(inventory.contains(item))
      run(s"drop $item")

  object Droid:

    def apply(cpu: CPU): Droid =
      val outputs   = cpu.outputStates
      val stdout    = outputs.map((_,value) => value.toChar).mkString
      val (next, _) = outputs.last

      stdout match
        case Room(room, doors, items, password2) =>
          val doorsSet = ListItem.findAllMatchIn(doors).map(_.group(1)).toSet
          val itemsSet = if items == null then Set.empty[String] else ListItem.findAllMatchIn(items).map(_.group(1)).toSet
          Droid(room, doorsSet, itemsSet, Set.empty, Option(password2).map(_.toInt))(next)

  private val badItems =
    Set(
      "escape pod",
      "infinite loop",
      "giant electromagnet",
      "photons",
      "molten lava",
    )

  private val opposite =
    Map(
      "north" -> "south",
      "south" -> "north",
      "east"  -> "west",
      "west"  -> "east",
    )

  private val securityRoom =
    "Security Checkpoint"

  def collectItems(droid: Droid, excludeDoor: Option[String] = None): Droid =
    val collected = droid.items.filterNot(badItems).foldLeft(droid)(_ take _)
    if droid.room == securityRoom then
      collected
    else
      collected
        .doors
        .filterNot(excludeDoor.contains)
        .foldLeft(collected): (collect, door) =>
          val back = opposite(door)
          collectItems(collect.move(door), Some(back)).move(back)

  def goToSecurityRoom(droid: Droid): Droid =

    def bfs(start: Droid): Option[Droid] =
      val todo  = collection.mutable.Queue(start)
      var found = Option.empty[Droid]

      while todo.nonEmpty && found.isEmpty do
        val current = todo.dequeue
        current
          .doors
          .iterator
          .map(current.move)
          .foreach: next =>
            if next.room == securityRoom then
              found = Some(next)
            else
              todo.enqueue(next)

      found

    bfs(droid).get


  def findPressureSensitiveFloorDoor(droidState: Droid): String =
    droidState.doors.find(droidState.move(_).room == securityRoom).get

  def findPassword(program: Mem): Int =

    val initial  = Droid(CPU(program))
    val items    = collectItems(initial)
    val security = goToSecurityRoom(items)
    val pressure = findPressureSensitiveFloorDoor(security)

    val password =
      security
        .inventory
        .subsets
        .map: dropped =>
          dropped.foldLeft(security)(_ drop _).move(pressure)
        .flatMap(_.password)
        .next

    password

  val start1  = System.currentTimeMillis
  lazy val answer1 = findPassword(program)
  println(s"Answer AOC 2019 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")
