package ua.gradsoft.epp

import scala.concurrent.{ExecutionContext, Future}
import ua.gradsoft.epp.rpc.EppTcpRpcServiceImpl
import ua.gradsoft.epp.xsdmodel._

class TcpClientEppService(
  config: EppConfig,
  rpcService: EppTcpRpcServiceImpl
)(implicit ec: ExecutionContext) extends EppService {

  require(config.transportType == EppTransportType.Tcp, "TcpClientEppService requires Tcp transport type")

  override def login(credentials: EppCredentials): Future[EppConnection] = {
    val loginType = LoginType(
      clID = credentials.id,
      pw = credentials.pw,
      newPW = None,
      options = CredsOptionsType(
        version = Number1u460,
        lang = "en"
      ),
      svcs = LoginSvcType(
        objURI = Seq(
          new java.net.URI("http://hostmaster.ua/epp/domain-1.1"),
          new java.net.URI("http://hostmaster.ua/epp/contact-1.1"),
          new java.net.URI("http://hostmaster.ua/epp/host-1.1")
        ),
        svcExtension = None
      )
    )

    rpcService.login(loginType).map { response =>
      response.result.headOption match {
        case Some(result) =>
          val resultCode = result.code
          resultCode match {
            case Number1000 | Number1500 =>
              new EppConnectionImpl(rpcService)
            case code =>
              throw ua.gradsoft.epp.rpc.EppErrorException(result.msg.value, code)
          }
        case None =>
          throw new RuntimeException("No result in login response")
      }
    }
  }

  def close(): Unit = rpcService.close()
}

object TcpClientEppService {

  def apply(config: EppConfig)(implicit ec: ExecutionContext): TcpClientEppService = {
    require(config.host.isDefined, "TcpClientEppService requires host to be defined")
    val host = config.host.get
    val port = config.port.getOrElse(700)

    val sslContext = config.clientCertificatePem match {
      case Some(pemPath) =>
        EppSslContext.fromPemFile(pemPath, config.serverCaPem)
      case None =>
        val ctx = javax.net.ssl.SSLContext.getInstance("TLS")
        config.serverCaPem match {
          case Some(caPemPath) =>
            val caPem = java.nio.file.Files.readString(java.nio.file.Path.of(caPemPath))
            val caCerts = extractAllCertificates(caPem)
            val trustStore = java.security.KeyStore.getInstance("PKCS12")
            trustStore.load(null, null)
            caCerts.zipWithIndex.foreach { (cert, idx) =>
              trustStore.setCertificateEntry(s"ca-$idx", cert)
            }
            val tmf = javax.net.ssl.TrustManagerFactory.getInstance(javax.net.ssl.TrustManagerFactory.getDefaultAlgorithm)
            tmf.init(trustStore)
            ctx.init(null, tmf.getTrustManagers, null)
            ctx
          case None =>
            ctx.init(null, null, null)
            ctx
        }
    }

    val rpcService = EppTcpRpcServiceImpl.connect(host, port, sslContext)
    new TcpClientEppService(config, rpcService)
  }

  private def extractAllCertificates(pem: String): Seq[java.security.cert.X509Certificate] = {
    val cf = java.security.cert.CertificateFactory.getInstance("X.509")
    val beginMarker = "-----BEGIN CERTIFICATE-----"
    val endMarker = "-----END CERTIFICATE-----"
    val certs = scala.collection.mutable.ArrayBuffer[java.security.cert.X509Certificate]()
    var searchFrom = 0
    while {
      val beginIdx = pem.indexOf(beginMarker, searchFrom)
      val endIdx = pem.indexOf(endMarker, searchFrom)
      if (beginIdx >= 0 && endIdx >= 0) {
        val base64 = pem.substring(beginIdx + beginMarker.length, endIdx).replaceAll("\\s", "")
        val certBytes = java.util.Base64.getDecoder.decode(base64)
        certs += cf.generateCertificate(new java.io.ByteArrayInputStream(certBytes)).asInstanceOf[java.security.cert.X509Certificate]
        searchFrom = endIdx + endMarker.length
        true
      } else {
        false
      }
    } do ()
    certs.toSeq
  }
}
