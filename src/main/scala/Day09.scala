import scala.io.Source

object Day09 extends App:

  val day: String = this.getClass.getName.drop(3).init

  enum Dir:
    case U
    case D
    case R
    case L

  import Dir.*

  case class Pos(x: Int, y: Int):
    def move(d: Dir): Pos =
      d match
        case U => copy(y = y + 1)
        case D => copy(y = y - 1)
        case L => copy(x = x - 1)
        case R => copy(x = x + 1)

    def diff(p: Pos): Pos =
      Pos(x - p.x, y - p.y)
      def diff(h: Int, t: Int): Int =
        if      h == t     then  0
        else if h == t + 1 then  1
        else if h == t - 1 then -1
        else sys.error(s"boom! : h=$h, t=$t")
      copy(diff(p.x, x), diff(p.y, y))
    
    def follow(d: Dir)(h: Pos): Pos =
      d match
        case U =>
          diff(h) match
            case Pos(-1, 1) => copy(x = x - 1, y = y + 1)
            case Pos( 0, 1) => copy(x = x    , y = y + 1)
            case Pos( 1, 1) => copy(x = x + 1, y = y + 1)
            case Pos(-1, 0) => this
            case Pos( 0, 0) => this
            case Pos( 1, 0) => this
            case Pos(-1,-1) => this
            case Pos( 0,-1) => this
            case Pos( 1,-1) => this
        case D =>
          diff(h) match
            case Pos(-1, 1) => this
            case Pos( 0, 1) => this
            case Pos( 1, 1) => this
            case Pos(-1, 0) => this
            case Pos( 0, 0) => this
            case Pos( 1, 0) => this
            case Pos(-1,-1) => copy(x = x - 1, y = y - 1)
            case Pos( 0,-1) => copy(x = x    , y = y - 1)
            case Pos( 1,-1) => copy(x = x + 1, y = y - 1)
        case R =>
          diff(h) match
            case Pos(-1, 1) => this
            case Pos( 0, 1) => this
            case Pos( 1, 1) => copy(x = x + 1, y = y + 1)
            case Pos(-1, 0) => this
            case Pos( 0, 0) => this
            case Pos( 1, 0) => copy(x = x + 1, y = y    )
            case Pos(-1,-1) => this
            case Pos( 0,-1) => this
            case Pos( 1,-1) => copy(x = x + 1, y = y - 1)
        case L =>
          diff(h) match
            case Pos(-1, 1) => copy(x = x - 1, y = y + 1)
            case Pos( 0, 1) => this
            case Pos( 1, 1) => this
            case Pos(-1, 0) => copy(x = x - 1, y = y    )
            case Pos( 0, 0) => this
            case Pos( 1, 0) => this
            case Pos(-1,-1) => copy(x = x - 1, y = y - 1)
            case Pos( 0,-1) => this
            case Pos( 1,-1) => this




  object Pos:
    def of(x: Int, y: Int): Pos =
      Pos(x,y)

    given Ordering[Pos] with
      def compare(a: Pos, b: Pos): Int =
        Ordering[(Int,Int)].compare((a.y, a.x), (b.y, b.x))

  case class Cmd(dir: Dir, steps: Int)

  val input: List[Cmd] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map {
        case s"U $s" => Cmd(U, s.toInt)
        case s"D $s" => Cmd(D, s.toInt)
        case s"L $s" => Cmd(L, s.toInt)
        case s"R $s" => Cmd(R, s.toInt)
      }
      .toList

  case class Bac(strep: List[Pos] = List.fill(2)(Pos.of(1000000,1000000))):
    assert(strep.forall(p => p.x > 0 && p.y > 0))
    def step(c: Cmd, s: List[Pos]): List[Pos] =
      def loop(todo: List[Pos], a: List[Pos]): List[Pos] =
        todo match
          case Nil    =>
            a
          case h :: t =>
            val p = s(a.size)
            val q = a :+ h.follow(c.dir)(p)
            loop(t, q)
      s.head.move(c.dir) :: loop(s.tail, List())

    def move(cmd: Cmd): List[Bac] =
      List.fill(cmd.steps)(cmd).foldLeft(List(this))((r,c) => {
        val nss = step(c, r.last.strep)
        r :+ Bac(nss)
      })

  val trace: List[Bac] =
    input.foldLeft(List(Bac()))((p,c) => p ++ p.last.move(c))


  val start1: Long  = System.currentTimeMillis
  val answer1: Int = trace.map(_.strep.last).distinct.size
  println(answer1)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long  = System.currentTimeMillis
  val answer2: Long = 666
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start1}ms]")
