object SumOfMultiples {
  def sum(factors: Set[Int], limit: Int): Int = {
    var total = 0
    for (i <- 1 to limit - 1) {
      if (isMultiple(factors, i)) {
        total += i
      }
    }
    total
  }

  def isMultiple(factors: Set[Int], candidate: Int) = {
    var result = false
    for (fact <- factors) {
      if (candidate % fact == 0) {
        result = true
      }
    }
    result
  }
}

