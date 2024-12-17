package nmcb

object Extensions:

  extension [A](self: Iterator[A]) def findMap[B](f: A => Option[B]): B =
    self.flatMap(f).next()
  
  extension (s: String) def leftPadTo(length: Int, char: Char) =
    List.fill(length - s.length)(char).mkString + s



