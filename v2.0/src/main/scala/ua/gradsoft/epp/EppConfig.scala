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
   * Wall-clock bound on one request/response exchange, and separately on waiting for a turn on
   * the connection - so a caller blocks for at most twice this. EPP over TCP keeps long-lived
   * sessions, and a session the registry has dropped - or one a firewall evicted while idle -
   * leaves the socket looking established while no reply can ever arrive. Because exchanges are
   * serialised on one connection, an unbounded read takes every later EPP operation with it, so
   * this covers the whole framed read and not just the gap between two bytes the way SO_TIMEOUT
   * alone would.
   */
  readTimeoutMillis: Int = 60000
)

object EppConfig {

  def tcp(host: String, port: Int = 700, clientCertificatePem: Option[String] = None, serverCaPem: Option[String] = None): EppConfig =
    EppConfig(EppTransportType.Tcp, host = Some(host), port = Some(port), clientCertificatePem = clientCertificatePem, serverCaPem = serverCaPem)

  def mock(): EppConfig =
    EppConfig(EppTransportType.Mock)
}
