package examples

object Magnus extends App:

  trait Order[A]:
    def gt(l: A, r: A): Boolean

  def sort[A : Order](list: List[A]): List[A] =

    def loop(list: List[A], n: Int): List[A] =
      println(s"list=$list, n=$n")
      if n == list.length - 1 then
        list
      else
        val l = list(n)
        val r = list(n + 1)
        if summon[Order[A]].gt(l, r) then
          val swapped = list.slice(0, n) :++ List(r) :++ List(l) :++ list.slice(n + 2, list.length)
          val next = n - 1
          loop(swapped, if next <= 0 then 0 else next)
        else
          loop(list, n + 1)

    loop(list, 0)

  given Order[Int] =
    new Order[Int]:
      override def gt(l: Int, r: Int): Boolean =
        l > r

  val data1: List[Int] =
    List(1, 9, 2, 8, 3, 7, 4, 6, 5, 1)

  println(s"data1=$data1")
  println(s"sort1=${sort(data1)}")


  case class Person(name: String, age: Int)

  object Person:

    given Order[Person] =
      new Order[Person]:
        override def gt(l: Person, r: Person): Boolean =
          l.age > r.age


  val data3: List[Person] =
    List(Person("Marco", 53), Person("Magnus", 19))

  println(s"data3=$data3")
  given Order[Person]:
    def gt(l: Person, r: Person): Boolean = l.name.length > r.name.length
  println(s"sort3=${sort(data3)}")




