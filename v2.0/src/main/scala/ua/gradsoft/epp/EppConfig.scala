package ua.gradsoft.epp

enum EppTransportType {
  case Https
  case Mock
}

case class EppConfig(
  transportType: EppTransportType,
  url: Option[String] = None
)

object EppConfig {

  def https(url: String): EppConfig =
    EppConfig(EppTransportType.Https, Some(url))

  def mock(): EppConfig =
    EppConfig(EppTransportType.Mock, None)
}
