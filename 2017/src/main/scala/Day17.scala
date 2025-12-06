object Day17 extends App:

  val day: String =
    getClass.getName.filter(_.isDigit).mkString("")

  case class Circle1(elements: Vector[Int], current: Int):

    infix def insert(steps: Int): Circle1 =
      val value             = elements(current) + 1
      val index             = (current + steps) % elements.size + 1
      val (prefix, postfix) = elements.splitAt(index)
      val nextElements      = prefix ++ (value +: postfix)
      copy(elements = nextElements, current = index)

    def nextValue: Int =
      val index = (current + 1) % elements.size
      elements(index)

  object Circle1:

    def init: Circle1 =
      Circle1(elements = Vector(0), current = 0)

    def spin(steps: Int, amount: Int): Circle1 =
      Iterator.iterate(Circle1.init)(_.insert(steps)).drop(amount).next()

  val start1: Long = System.currentTimeMillis
  lazy val answer1: Int = Circle1.spin(370, 2017).nextValue
  println(s"Answer AOC 2017 day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  case class Circle2(size: Int, value: Int, current: Int):

    infix def insert(steps: Int): Circle2 =
      val index = (current + steps) % size + 1
      copy(
        size    = size + 1,
        value   = if index == 1 then size else value,
        current = index
      )

  object Circle2:

    def init: Circle2 =
      Circle2(size = 1, value = 0, current = 0)

    def spin(steps: Int, amount: Int): Circle2 =
      Iterator.iterate(Circle2.init)(_.insert(steps)).drop(amount).next()


  val start2: Long = System.currentTimeMillis
  lazy val answer2: Int = Circle2.spin(370, 50000000).value
  println(s"Answer AOC 2017 day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
