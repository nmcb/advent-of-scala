package nmcb

object extensions:

  extension [A](self: Iterator[A]) def findMap[B](f: A => Option[B]): B =
    self.flatMap(f).next()
  
  extension (s: String) def leftPadTo(length: Int, char: Char) =
    List.fill(length - s.length)(char).mkString + s

  extension [A](tuple: (Pos,A))
    def pos: Pos = tuple._1
    def element: A = tuple._2