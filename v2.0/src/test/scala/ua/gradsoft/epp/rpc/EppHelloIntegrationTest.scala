package ua.gradsoft.epp.rpc

import org.scalatest.Tag
import org.scalatest.funsuite.AnyFunSuite
import ua.gradsoft.epp.EppSslContext

import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

object IntegrationTest extends Tag("IntegrationTest")

class EppHelloIntegrationTest extends AnyFunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  private def findTestResource(name: String): String = {
    val candidates = Seq(
      java.nio.file.Path.of(s"src/test/resource/$name"),
      java.nio.file.Path.of(s"../../src/test/resource/$name")
    )
    candidates.find(java.nio.file.Files.exists(_))
      .map(_.toAbsolutePath.toString)
      .getOrElse(sys.error(s"$name not found; provide path via system property"))
  }

  test("hello() returns a valid greeting from epp-test.hostmaster.ua via TCP", IntegrationTest) {
    val pemPath = sys.props.getOrElse("epp.clientCertificatePem", findTestResource("client-cert.pem"))
    val caPath = sys.props.getOrElse("epp.serverCaPem", findTestResource("hostmaster-ca.pem"))
    val sslContext = EppSslContext.fromPemFile(pemPath, Some(caPath))
    val host = sys.props.getOrElse("epp.host", "epp-test.hostmaster.ua")
    val port = sys.props.getOrElse("epp.port", "700").toInt

    val rpcService = EppTcpRpcServiceImpl.connect(host, port, sslContext)
    try {
      val greeting = Await.result(rpcService.hello(), 30.seconds)

      assert(greeting.svID.nonEmpty, "Server ID should not be empty")
      assert(greeting.svcMenu.version.nonEmpty, "Service menu should have at least one version")
      assert(greeting.svcMenu.lang.nonEmpty, "Service menu should have at least one language")
    } finally {
      rpcService.close()
    }
  }
}
