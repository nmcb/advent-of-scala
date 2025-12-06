import scala.annotation.*
import scala.io.*

object Day11 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  val input: String =
    "hxbxwxba"

  /** Modeling */

  case class Dig(value: Char = 'a'):
    import Dig.*
    import Carry.*

    assert((MinValue to MaxValue).contains(value), s"illegal digit value=${value.toInt}")

    override def toString: String =
      value.toString

    def inc: Option[Dig] =
      val (n, c) = inc(Zero)
      if c == Zero then Some(n) else None

    def inc(carry: Carry = Zero): (Dig, Carry) =
      assert(carry == Zero || carry == One)
      val n = value + One.value + carry.value
      ( if n <= MaxValue then copy(value = n.toChar) else copy(value = (n - ValueRange).toChar)
      , if n <= MaxValue then Zero                   else One
      )

  object Dig:

    enum Carry(val value: Int):
      case Zero extends Carry(0x00)
      case One  extends Carry(value = 0x01)

    val MinValue: Char   = 'a'
    val MaxValue: Char   = 'z'
    val ValueRange: Char = (MaxValue - MinValue + 1).toChar

    extension (c: Char) def toDig: Dig =
      Dig(c)


  case class Password(num: Array[Dig]):
    import Dig.*
    import Carry.*
    import Password.*

    assert(num.size == 8)

    override def toString: String =
      num.mkString

    @tailrec final def next: Password =
      @tailrec def inc(todo: Array[Dig], carry: Carry = Zero, acc: Array[Dig] = Array.empty): (Array[Dig], Carry) =
        if todo.isEmpty then
          (acc, carry)
        else
          val (n, c) = todo.last.inc(carry)
          if c == Zero then
            (todo.init ++: n +: acc, Zero)
          else
            inc(todo.init, Zero, n +: acc)

      val (n, c) = inc(num)
      val result = if c == Zero then Password(n) else Password.empty
      if result.valid then result else result.next

    def includesIncreasingStraight: Boolean =
      num
        .sliding(3)
        .exists(ds => ds(0).inc.contains(ds(1)) && ds(1).inc.contains(ds(2)))

    def includesLegalCharacters: Boolean =
      num.forall(d => d.value != 'i' && d.value != 'o' && d.value != 'l')

    def includesNonOverlappingPair: Boolean =
      def hasNonOverlappingPair(todo: String, foundFirst: Boolean = false): Boolean =
        todo.toList match
          case c0 :: c1 :: _ if c0 == c1 && foundFirst => true
          case c0 :: c1 :: t if c0 == c1               => hasNonOverlappingPair(t.mkString, true)
          case  _ :: rest                              => hasNonOverlappingPair(rest.mkString, foundFirst)
          case Nil                                     => false

      hasNonOverlappingPair(num.show)

    def valid: Boolean =
      includesIncreasingStraight && includesLegalCharacters && includesNonOverlappingPair

  object Password:

    import Dig.*

    def empty: Password =
      Password.fromString("aaaaaaaa")

    def fromString(string: String): Password =
      assert(string.length == 8)
      assert(string.forall(('a' to 'z').contains))
      Password(string.foldLeft(Array.empty[Dig])(_ :+ _.toDig))

    extension (num: Array[Dig]) def show: String =
      num.mkString


  /** Part 1 */

  val start1: Long = System.currentTimeMillis
  lazy val answer1: String = Password.fromString(input).next.toString
  println(s"Answer AOC 2015 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")


  /** Part 2 */

  val start2: Long = System.currentTimeMillis
  lazy val answer2: String = Password.fromString(answer1).next.toString
  println(s"Answer AOC 2015 day $day part 2: ${answer2} [${System.currentTimeMillis - start2}ms]")
