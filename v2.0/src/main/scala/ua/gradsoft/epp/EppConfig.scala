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
  serverCaPem: Option[String] = None,
  /**
   * Bound on how long a read may block. EPP over TCP keeps long-lived sessions, and a session the
   * registry has dropped - or one a firewall evicted while idle - leaves the socket looking
   * established while no reply can ever arrive. Without this a read blocks forever and, because
   * the request/response pair is serialised on one monitor, takes every later EPP operation with it.
   */
  readTimeoutMillis: Int = 60000
)

object EppConfig {

  def tcp(host: String, port: Int = 700, clientCertificatePem: Option[String] = None, serverCaPem: Option[String] = None): EppConfig =
    EppConfig(EppTransportType.Tcp, host = Some(host), port = Some(port), clientCertificatePem = clientCertificatePem, serverCaPem = serverCaPem)

  def mock(): EppConfig =
    EppConfig(EppTransportType.Mock)
}
