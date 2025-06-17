import scala.io.Source

object Day22 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class Pos(x: Int, y: Int)

  case class Node(used: Int, avail: Int):
    def isEmpty: Boolean            = used == 0
    def nonEmpty: Boolean           = !isEmpty
    def fitsOn(that: Node): Boolean = that.avail >= used
    def isMassive                   = used > 100

    def toChar: Char =
      if      isEmpty   then 'E'
      else if isMassive then '#'
      else                   '.'

  val nodes: Map[Pos,Node] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .filter(_.startsWith("/dev/grid/node"))
      .map: line =>
        val Array(x, y, _, used, avail, _) = line.split("\\D+").tail.map(_.toInt)
        Pos(x, y) -> Node(used, avail)
      .toMap

  def viable(nodes: Map[Pos,Node]): Vector[(Node,Node)] =
    for
      (_,a) <- nodes.toVector
      (_,b) <- nodes.toVector
      if a != b && a.nonEmpty && a.fitsOn(b)
    yield
      (a,b)

  extension (location: (Pos, Node))
    def pos:   Pos = location._1
    def node: Node = location._2

  val start1  = System.currentTimeMillis
  val answer1 = viable(nodes).size
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")


  extension (nodes: Map[Pos, Node])
    def maxX = nodes.maxBy(_.pos.x).pos.x
    def maxY = nodes.maxBy(_.pos.y).pos.y

    def asString: String =
      val sb = StringBuilder(maxX * maxY)
      for
        y <- 0 to maxY
        x <- 0 to maxX
      do
        val pos = Pos(x, y)
        val node = nodes(pos)
        if pos == Pos(0, 0) then sb.append('T')
        else if pos == Pos(maxX, 0) then sb.append('S')
        else sb.append(node.toChar)
        if x == maxX then sb.append('\n')
      sb.append('\n').toString

  /**
   * After some inspection of the grid we find out that there are some nodes
   * larger than 100T that are so massive that their data cannot be moved to
   * other nodes; and, that there is only one free node in the grid with the
   * space available to accommodate the data present on other (non-massive)
   * nodes; and, that the layout of the grid looks like this:
   *
   * println(nodes.asString)
   *
   * T.............................S
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * .....##########################
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * ...............................
   * .............E.................
   * ...............................
   * ...............................
   * ...............................
   *
   * Where:
   *
   * S = The source node whose data needs to be moved to node T
   * T = The target node for the data of node S
   * E = The empty node
   * # = The massive nodes which data can never be moved
   *
   */

  val source  = Pos(nodes.maxX, 0)
  val target  = Pos(0, 0)
  val empty   = nodes.find(_.node.isEmpty).map(_.pos).get
  val massive = nodes.filter(_.node.isMassive).map(_.pos)

  /**
   * This brings the problem back to the following two sub-problems this time
   * in terms of node movements instead of data movements:
   *
   * 1) Move the empty node E to the node left of source S
   * 2) Move the source node S to target node T
   *
   * Problem 1 is simple and takes 1 data move per node move. We just need to walk
   * around the wall of massive nodes, we're able to do so on the left side. As we
   * can only walk along the axis we can compute the number of steps in terms of x
   * and y coordinates using manhattan distance.
   */

  val left   = massive.minBy(_.x)
  val steps1 = (empty.x - left.x + 1) + (source.x - left.x + 1) + empty.y

  /**
   * Problem 2 is a little bit more involved but still rather simple. At the start of
   * this problem (after problem 1) the current situation in the upper side of the grid
   * is as shown below:
   *
   * T............................SE
   * ...............................
   * ...............................
   *
   * Note that we need 5 node moves for one data move as the empty node needs to move
   * from the right side of the current source node to the left side in order to move
   * the data from that source node to the left. I.e.:
   *
   * move 1:
   *
   * T............................S.
   * ..............................E
   * ...............................
   *
   * move 2:
   *
   * T............................S.
   * .............................E.
   * ...............................
   *
   * move 3:
   *
   * T............................S.
   * ............................E..
   * ...............................
   *
   * move 4:
   *
   * T...........................ES.
   * ...............................
   * ...............................
   *
   * move 5:
   *
   * T...........................SE.
   * ...............................
   * ...............................
   *
   * This means that we need 5 steps to move the source data one node closer to the
   * target node with the source node for problem 2 starting at max x - 1. This can
   * be calculated:
   */

  val steps2 = (nodes.maxX - 1) * 5

  /**
   *
   * Voila!
   *
   */

  val start2  = System.currentTimeMillis
  val answer2 = steps1 + steps2
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")
