package ua.gradsoft.epp

import scala.concurrent.{ExecutionContext, Future}
import ua.gradsoft.epp.rpc.MockEppRpcService
import ua.gradsoft.epp.xsdmodel._

class FakeTestEppService(implicit ec: ExecutionContext) extends EppService {

  private val mockRpcService = new MockEppRpcService()

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

    mockRpcService.login(loginType).map { response =>
      response.result.headOption match {
        case Some(result) =>
          val resultCode = result.code
          resultCode match {
            case Number1000 | Number1500 =>
              new EppConnectionImpl(mockRpcService)
            case code =>
              throw ua.gradsoft.epp.rpc.EppErrorException(result.msg.value, code)
          }
        case None =>
          throw new RuntimeException("No result in login response")
      }
    }
  }
}

object FakeTestEppService {

  def apply()(implicit ec: ExecutionContext): FakeTestEppService = {
    new FakeTestEppService()
  }

  def apply(config: EppConfig)(implicit ec: ExecutionContext): FakeTestEppService = {
    require(config.transportType == EppTransportType.Mock, "FakeTestEppService requires Mock transport type")
    new FakeTestEppService()
  }
}
