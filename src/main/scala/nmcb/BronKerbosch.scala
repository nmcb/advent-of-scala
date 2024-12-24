package nmcb

object BronKerbosch:

  /** Computes the maximum clique, i.e. the largest group of neighbouring nodes. */
  def run[A](neighbours: Map[A,Set[A]]): Set[A] =
    var maximum: Set[A] = Set.empty

    def loop(r: Set[A], p: Set[A], x: Set[A]): Unit =
      if p.isEmpty && x.isEmpty then
        if r.size > maximum.size then maximum = r
      else
        val u  = (p union x).maxBy(neighbours(_).size)
        var p2 = p
        var x2 = x
        for
          v <- p diff neighbours(u)
        yield
          loop(r + v, p intersect neighbours(v), x intersect neighbours(v))
          p2 -= v
          x2 += v

    loop(Set.empty, neighbours.keySet, Set.empty)
    maximum
