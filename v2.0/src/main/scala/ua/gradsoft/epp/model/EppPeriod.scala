package ua.gradsoft.epp.model

case class EppPeriod(value: Int, unit: String)

object EppPeriod {
  /**
   * Creates an EppPeriod instance with the unit set to "y" (years).
   * @param years The number of years for the period.
   * @return An EppPeriod instance.
   */
  def fromYears(years: Int): EppPeriod = EppPeriod(years, "y")
}
