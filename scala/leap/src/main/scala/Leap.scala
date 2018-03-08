object Leap {
  def leapYear(year: Int): Boolean =
    if      (mult(year, 400)) true
    else if (mult(year, 100)) false
    else     mult(year, 4)

  def mult(value: Int, factor: Int) = value % factor == 0
}
