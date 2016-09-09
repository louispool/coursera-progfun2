package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double], c: Signal[Double]): Signal[Double] = {
    Signal(b()*b() - 4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal[Set[Double]] {
      val d = delta()
      if (d < 0) Set.empty[Double]
      else {
        val root = Math.sqrt(d)
        val a2 = 2*a()

        Set((-b() + root) / a2, (-b() - root) / a2)
      }
    }
  }
}
