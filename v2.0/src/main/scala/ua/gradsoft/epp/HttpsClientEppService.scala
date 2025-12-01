package ua.gradsoft.epp

import scala.concurrent.{ExecutionContext, Future}
import sttp.client4._
import ua.gradsoft.epp.rpc.EppHttpsRpcServiceImpl
import ua.gradsoft.epp.xsdmodel._

class HttpsClientEppService(
  config: EppConfig,
  backend: Backend[Future]
)(implicit ec: ExecutionContext) extends EppService {

  require(config.transportType == EppTransportType.Https, "HttpsClientEppService requires Https transport type")
  require(config.url.isDefined, "HttpsClientEppService requires URL to be defined")

  private val rpcService = new EppHttpsRpcServiceImpl(config.url.get, backend)

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
}

object HttpsClientEppService {

  def apply(config: EppConfig)(implicit ec: ExecutionContext): HttpsClientEppService = {
    val backend = DefaultFutureBackend()
    new HttpsClientEppService(config, backend)
  }

  def apply(config: EppConfig, backend: Backend[Future])(implicit ec: ExecutionContext): HttpsClientEppService = {
    new HttpsClientEppService(config, backend)
  }

  def apply(url: String)(implicit ec: ExecutionContext): HttpsClientEppService = {
    apply(EppConfig.https(url))
  }
}
