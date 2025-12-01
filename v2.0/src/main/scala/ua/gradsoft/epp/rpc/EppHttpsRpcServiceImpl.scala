package ua.gradsoft.epp.rpc

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}
import sttp.client4._
import sttp.model.MediaType
import ua.gradsoft.epp.xsdmodel._

/**
 * EPP RPC Service implementation that communicates over HTTPS.
 *
 * @param url The HTTPS URL of the EPP server endpoint
 * @param backend The sttp backend to use for HTTP requests
 * @param executionContext The execution context for async operations
 */
class EppHttpsRpcServiceImpl(
  val url: String,
  val backend: Backend[Future]
)(implicit val executionContext: ExecutionContext) extends EppRpcServiceImpl {

  private val eppContentType = MediaType("application", "epp+xml")

  override def processEppMessage(input: EppType): Future[EppType] = {
    val xmlRequest = serializeEppType(input)
    val request = basicRequest
      .post(uri"$url")
      .contentType(eppContentType)
      .body(xmlRequest)
      .response(asString)

    request.send(backend).flatMap { response =>
      response.body match {
        case Right(xmlString) =>
          deserializeEppType(xmlString) match {
            case Success(eppResponse) => Future.successful(eppResponse)
            case Failure(e) => Future.failed(
              new IllegalArgumentException(s"Failed to parse EPP response: ${e.getMessage}", e)
            )
          }
        case Left(error) =>
          Future.failed(new RuntimeException(s"HTTP request failed: $error"))
      }
    }
  }

  private def serializeEppType(eppType: EppType): String = {
    val xmlNodes = scalaxb.toXML[EppType](eppType, Some("urn:ietf:params:xml:ns:epp-1.0"), Some("epp"), defaultScope)
    val xmlElem = xmlNodes.head.asInstanceOf[Elem]
    val xmlWithDeclaration = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + xmlElem.toString()
    xmlWithDeclaration
  }

  private def deserializeEppType(xmlString: String): Try[EppType] = {
    Try {
      val xmlElem = XML.loadString(xmlString)
      scalaxb.fromXML[EppType](xmlElem)
    }
  }
}

object EppHttpsRpcServiceImpl {

  /**
   * Create an EppHttpsRpcServiceImpl with the default Future backend.
   */
  def apply(url: String)(implicit ec: ExecutionContext): EppHttpsRpcServiceImpl = {
    val backend = DefaultFutureBackend()
    new EppHttpsRpcServiceImpl(url, backend)
  }

  /**
   * Create an EppHttpsRpcServiceImpl with a custom backend.
   */
  def apply(url: String, backend: Backend[Future])(implicit ec: ExecutionContext): EppHttpsRpcServiceImpl = {
    new EppHttpsRpcServiceImpl(url, backend)
  }
}
