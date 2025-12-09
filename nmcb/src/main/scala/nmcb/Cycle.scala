package nmcb

import predef.*

case class Cycle[A](stemSize: Int, cycleSize: Int, cycleHead: A, cycleLast: A, cycleHeadRepeat: A, next: A => A):

  def simulate(n: Long): A =
    val iterations = (n - stemSize) % cycleSize
    Iterator.iterate(cycleHeadRepeat)(next).nth(iterations.toInt)


object Cycle:

  import scala.collection.*

  extension [A](i: Iterator[A])
    private def zipWithPrevious: Iterator[(A,Option[A])] =
      new AbstractIterator[(A,Option[A])]:

        private var last: Option[A] =
          None

        override def hasNext: Boolean =
          i.hasNext

        override def next: (A,Option[A]) =
          val current  = i.next
          val previous = last
          last = Some(current)
          (current, previous)

  def find[A,B](a: A, next: A => A, invariant: A => B = identity): Cycle[A] =

    val trace: mutable.Map[B,(A,Int)] = mutable.Map[B,(A,Int)]()

    Iterator.iterate(a)(next)
      .iterator
      .zipWithPrevious
      .zipWithIndex
      .map:
        case ((current, previous), index) =>
          (current, previous, trace.put(invariant(current), (current,index)), index)
      .collectFirst:
        case (current, Some(previous), Some(first,firstIndex), index) =>
          Cycle(
            stemSize        = firstIndex,
            cycleSize       = index - firstIndex,
            cycleHead       = first,
            cycleLast       = previous,
            cycleHeadRepeat = current,
            next            = next
          )
      .get