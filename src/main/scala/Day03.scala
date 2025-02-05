object Day03 extends App:

  enum Dir:
    case N, E, S, W

  import Dir.*

  case class Pos(x: Int, y: Int, n: Int):

    def step(d: Dir): Pos =
      d match
        case N => copy(y = y - 1, n = n + 1)
        case E => copy(x = x + 1, n = n + 1)
        case S => copy(y = y + 1, n = n + 1)
        case W => copy(x = x - 1, n = n + 1)

    infix def manhattan(p: Pos): Int =
      (x - p.x).abs + (y - p.y).abs

  case class Spiral(positions: Vector[Pos]):

    val minX: Int = positions.map(_.x).min
    val maxX: Int = positions.map(_.x).max
    val minY: Int = positions.map(_.y).min
    val maxY: Int = positions.map(_.y).max

    def growInclude(i: Int): Spiral =
      def loop(s: Spiral): Spiral =

        def eborder(c: Pos, a: Vector[Pos] = Vector.empty): Vector[Pos] =
          if c.y <= s.minY - 1 then a :+ c else eborder(c.step(N), a :+ c)

        def nborder(c: Pos, a: Vector[Pos] = Vector.empty): Vector[Pos] =
          if c.x <= s.minX - 1 then a :+ c else nborder(c.step(W), a :+ c)

        def wborder(c: Pos, a: Vector[Pos] = Vector.empty): Vector[Pos] =
          if c.y >= s.maxY + 1 then a :+ c else wborder(c.step(S), a :+ c)

        def sborder(c: Pos, a: Vector[Pos] = Vector.empty): Vector[Pos] =
          if c.x >= s.maxX + 1 then a :+ c else sborder(c.step(E), a :+ c)

        if s.positions.last.n >= i then
          s
        else
          val eb = eborder(s.positions.last.step(E))
          val nb = nborder(eb.last.step(W))
          val wb = wborder(nb.last.step(S))
          val sb = sborder(wb.last.step(E))
          loop(Spiral(s.positions :++ eb :++ nb :++ wb :++ sb))

      loop(this)

    def carry(i: Int): Int =
      val square = growInclude(i).positions.find(_.n == i).get
      val access = positions.find(_.n == 1).get
      square manhattan access

  object Spiral:
    def make: Spiral =
      Spiral(Vector(Pos(0,0,1)))

  val day: String = this.getClass.getName.filter(_.isDigit).mkString("")

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Spiral.make.carry(277678)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
