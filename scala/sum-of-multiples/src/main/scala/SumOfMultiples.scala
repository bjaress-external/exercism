object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    val multiples = for (i <- 1 to limit - 1
      if isMultiple(factors, i)) yield i
    multiples.sum
  }

  def isMultiple(factors: Set[Int], candidate: Int) =
    factors.exists(candidate % _ == 0)
}

