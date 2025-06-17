import scala.io.*

object Day07 extends App:

  val day = getClass.getSimpleName.filter(_.isDigit).mkString

  case class IP(supernets: Vector[String], hypernets: Vector[String]):

    private def isAbba(s: String): Boolean =
      s.length == 4 && s(0) != s(1) && s == s.reverse

    def hasTLS: Boolean =
      supernets.exists(_.sliding(4).exists(isAbba)) && !hypernets.exists(_.sliding(4).exists(isAbba))

    private def isAba(s: String, containing: Option[(Char,Char)] = None): Option[(Char,Char)] =
      containing match
        case None =>
          Option.when(s.length == 3 && s(0) != s(1) && s == s.reverse)((s(0), s(1)))
        case Some((outer,inner)) =>
          Option.when(s.length == 3 && s(0) != s(1) && s == s.reverse && s(0) == outer && s(1) == inner)((s(0), s(1)))

    def hasSSL: Boolean =
      val abas = supernets.flatMap(_.sliding(3).flatMap(isAba(_)))
      val babs = abas.map(_.swap)
      babs.nonEmpty && hypernets.exists(net => babs.exists(bab => net.sliding(3).exists(chars => isAba(chars, Some(bab)).isDefined)))


  object IP:

    def fromString(s: String): IP =
      def loop(todo: String, supernet: Boolean = true, supernets: Vector[String] = Vector.empty, hypernets: Vector[String] = Vector.empty): IP =
        val chars = if supernet then todo.takeWhile(_ != '[') else todo.takeWhile(_ != ']')
        val rest  = todo.drop(chars.length + 1)
        val nsupernets = if  supernet then supernets  :+ chars else supernets
        val nhypernets = if !supernet then hypernets :+ chars else hypernets
        if rest.length > 1 then
          loop(rest, !supernet, nsupernets, nhypernets)
        else
          IP(nsupernets, nhypernets)
      loop(s)

  val ips: Vector[IP] =
    Source
      .fromResource(s"input$day.txt")
      .getLines
      .map(IP.fromString)
      .toVector

  val start1: Long = System.currentTimeMillis
  val answer1: Int = ips.count(_.hasTLS)
  println(s"Answer day $day part 1: $answer1 [${System.currentTimeMillis - start1}ms]")

  val start2: Long = System.currentTimeMillis
  val answer2: Int = ips.count(_.hasSSL)
  println(s"Answer day $day part 2: $answer2 [${System.currentTimeMillis - start2}ms]")