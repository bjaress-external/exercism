object SpaceAge {
  val secondsPerEarthYear = 31557600

  def planetYears(earthYearsPerPlanetYear: Double)(seconds: Double): Double =
    round(seconds / secondsPerEarthYear / earthYearsPerPlanetYear)

  def round(raw: Double): Double =
    (raw * 100).round / 100.toDouble

  // I find this syntax wacky, but the compiler literally told me to.
  val onEarth = planetYears(1) _
  val onMercury = planetYears(0.2408467) _
  val onVenus = planetYears(0.61519726) _
  val onMars = planetYears(1.8808158) _
  val onJupiter = planetYears(11.862615) _
  val onSaturn = planetYears(29.447498) _
  val onUranus = planetYears(84.016846) _
  val onNeptune = planetYears(164.79132) _

}
