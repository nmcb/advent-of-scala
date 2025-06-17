import scala.io.Source

object Day08 extends App:

  val day: String = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Node(childs: List[Node], meta: List[Int]):
    def nrOfChilds: Int     = childs.size
    def nrOfMeta: Int       = meta.size
    def licenceNumber: Int  = meta.sum + childs.map(_.licenceNumber).sum
    def value: Int          = if childs.isEmpty then meta.sum else meta.map(idx => childs.lift(idx - 1).map(_.value).getOrElse(0)).sum

  object Node:

    def parseTree(s: String): Node =
      def loop(input: List[Int], todoChildren: Int, todoMeta: Int, children: List[Node] = List.empty): (Node, List[Int]) =
        if todoChildren == 0 then
          (Node(children, input.take(todoMeta)), input.drop(todoMeta))
        else
          // parse children
          val todoChildChildren = input.head
          val todoChildMeta = input.drop(1).head
          val (child, rest) = loop(input.drop(2), todoChildChildren, todoChildMeta, List.empty)
          loop(rest, todoChildren - 1, todoMeta, children :+ child)

      val input = s.trim.split(' ').map(_.toInt).toList
      val todoChildren = input.head
      val todoMeta = input.drop(1).head
      val (root, rest) = loop(input.drop(2), todoChildren, todoMeta)
      assert(rest.isEmpty)
      root

  val input = Source.fromResource(s"input$day.txt").mkString

  val start1: Long = System.currentTimeMillis
  val answer1: Int = Node.parseTree(input).licenceNumber
  println(s"Answer AOC 2018 day $day part 1: ${answer1} [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = Node.parseTree(input).value
  println(s"Answer AOC 2018 day $day part 2: ${answer2} [${System.currentTimeMillis - start1}ms]")
