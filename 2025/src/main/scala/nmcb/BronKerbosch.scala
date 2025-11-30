package nmcb

object BronKerbosch:

  /** Computes the maximum clique, i.e. the largest group of neighbouring nodes. */
  def run[A](neighbours: Map[A,Set[A]]): Set[A] =
    var maximum: Set[A] = Set.empty

    def loop(r: Set[A], p: Set[A], x: Set[A]): Unit =
      if p.isEmpty && x.isEmpty then
        if r.size > maximum.size then maximum = r
      else
        // Technically this implements Tomita's optimisation which recurses with a pivot node
        // chosen to contain the maximum number of edges. This variant of the Bron-Kerbosch
        // algorithm performs up to twice as fast on sparse, and similar on dense graphs.
        val u  = (p union x).maxBy(neighbours(_).size)
        var bp = p
        var bx = x
        for
          v <- p diff neighbours(u)
        yield
          loop(r + v, p intersect neighbours(v), x intersect neighbours(v))
          bp -= v
          bx += v

    loop(Set.empty, neighbours.keySet, Set.empty)
    maximum
