package ua.gradsoft.epp

enum EppTransportType {
  case Tcp
  case Mock
}

case class EppConfig(
  transportType: EppTransportType,
  host: Option[String] = None,
  port: Option[Int] = None,
  clientCertificatePem: Option[String] = None,
  serverCaPem: Option[String] = None
)

object EppConfig {

  def tcp(host: String, port: Int = 700, clientCertificatePem: Option[String] = None, serverCaPem: Option[String] = None): EppConfig =
    EppConfig(EppTransportType.Tcp, host = Some(host), port = Some(port), clientCertificatePem = clientCertificatePem, serverCaPem = serverCaPem)

  def mock(): EppConfig =
    EppConfig(EppTransportType.Mock)
}
