import scala.io.*

object Day26 extends App:

  val day: String = this.getClass.getName.drop(3).init

  def decompose(number: Vector[Char]): Vector[Vector[Char]] =
    def loop(number: Vector[Char], acc: Vector[Vector[Char]]): Vector[Vector[Char]] =
      number match
        case h +: t if acc.last.contains(h) => loop(t, acc.init :+ (acc.last :+ h))
        case h +: t                         => loop(t, acc :+ (Vector.empty :+ h))
        case _                              => acc
    if number.isEmpty then Vector.empty else loop(number.tail, Vector(Vector(number.head)))

  def successor(n: Vector[Char]): Vector[Char] =
    decompose(n).flatMap(c => Vector(c.size.toString.head, c.head))


  var start1: Long = System.currentTimeMillis
  Iterator.iterate(Vector('1'))(successor).foreach(s => {
    println(s"Answer day $day part 1: ${s.length} [${System.currentTimeMillis - start1}ms]")
    start1 = System.currentTimeMillis
  })
