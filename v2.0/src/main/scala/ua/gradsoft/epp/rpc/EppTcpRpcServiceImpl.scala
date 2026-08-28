package ua.gradsoft.epp.rpc

import java.io.{DataInputStream, DataOutputStream}
import javax.net.ssl.{SSLContext, SSLSocket}
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}
import scalaxb.DataRecord
import ua.gradsoft.epp.xsdmodel._

/**
 * EPP RPC Service implementation that communicates over TCP with RFC 5734 framing.
 *
 * RFC 5734 defines a 4-byte big-endian length prefix where the length includes
 * the 4 header bytes themselves: total_length = 4 + payload_length.
 *
 * @param sslSocket The connected TLS socket
 * @param initialGreeting The greeting received on connection (server sends it immediately)
 * @param executionContext The execution context for async operations
 */
class EppTcpRpcServiceImpl(
  val sslSocket: SSLSocket,
  val initialGreeting: EppType
)(implicit val executionContext: ExecutionContext) extends EppRpcServiceImpl {

  private val input = new DataInputStream(sslSocket.getInputStream)
  private val output = new DataOutputStream(sslSocket.getOutputStream)
  private val lock = new Object

  override def processEppMessage(request: EppType): Future[EppType] = {
    Future {
      blocking {
        lock.synchronized {
          writeFrame(serializeEppType(request))
          val responseBytes = readFrame()
          EppTcpRpcServiceImpl.parseEppType(new String(responseBytes, "UTF-8")) match {
            case Success(epp) => epp
            case Failure(e) => throw new IllegalArgumentException(
              s"Failed to parse EPP response: ${e.getMessage}", e
            )
          }
        }
      }
    }
  }

  def close(): Unit = {
    Try(sslSocket.close())
  }

  /** Blanks <pw> elements - the login frame and every authInfo carry a password in cleartext. */
  private def redact(xml: String): String =
    xml.replaceAll("(?s)(<(?:[\\w.-]+:)?pw>)[^<]*(</(?:[\\w.-]+:)?pw>)", "$1***$2")

  private def writeFrame(xml: String): Unit = {
    // Through the logger, not stderr: level-controlled, and it lands in the configured appender
    // with the rest of the application rather than in the journal outside any rotation policy.
    eppLogger.debug(s"EPP >>> ${redact(xml)}")
    val payload = xml.getBytes("UTF-8")
    val totalLength = 4 + payload.length
    output.writeInt(totalLength)
    output.write(payload)
    output.flush()
  }

  private def readFrame(): Array[Byte] = {
    // A timed-out read leaves the stream mid-frame: a late reply would be parsed as the next
    // response and desync every command after it. Close the socket so this connection is
    // discarded and the caller reconnects rather than reusing a stream it cannot trust.
    def abortOnTimeout[T](body: => T): T =
      try body
      catch {
        case e: java.net.SocketTimeoutException =>
          // Read the timeout first: getSoTimeout throws SocketException on a closed socket, which
          // would replace this SocketTimeoutException and lose both the message and the type
          // callers match on.
          val waitedMs = Try(sslSocket.getSoTimeout).getOrElse(0)
          Try(sslSocket.close())
          val timeout = new java.net.SocketTimeoutException(s"No EPP response within ${waitedMs}ms; connection closed")
          timeout.initCause(e)
          throw timeout
      }

    val totalLength = abortOnTimeout(input.readInt())
    import EppTcpRpcServiceImpl.{HeaderSize, MaxFrameSize}
    if (totalLength < HeaderSize || totalLength > MaxFrameSize) {
      // A desynced stream reads arbitrary bytes as a length. Without an upper bound the
      // allocation below reaches ~2GB and takes the JVM down instead of just this connection.
      Try(sslSocket.close())
      throw new IllegalArgumentException(
        s"Invalid EPP frame: total length $totalLength outside [$HeaderSize, $MaxFrameSize]; connection closed"
      )
    }
    val payloadLength = totalLength - 4
    val payload = new Array[Byte](payloadLength)
    abortOnTimeout(input.readFully(payload))
    eppLogger.debug(s"EPP <<< ${redact(new String(payload, "UTF-8"))}")
    payload
  }

  private def serializeEppType(eppType: EppType): String = {
    val xmlNodes = scalaxb.toXML[EppType](eppType, Some("urn:ietf:params:xml:ns:epp-1.0"), Some("epp"), defaultScope)
    val xmlElem = xmlNodes.head.asInstanceOf[Elem]
    """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + xmlElem.toString()
  }

}

object EppTcpRpcServiceImpl {

  private val HeaderSize = 4

  /** EPP frames are kilobytes; anything larger means the stream is not carrying EPP any more. */
  private val MaxFrameSize = 1024 * 1024

  /**
   * Parse an EPP XML string into EppType, dispatching by child element label.
   *
   * The scalaxb-generated EppType parser has a bug: `any(_ => true)` fires before
   * the typed alternatives, storing a raw Elem in DataRecord. Downstream record.as[T]
   * then fails with ClassCastException. This method bypasses the generated parser.
   */
  def parseEppType(xmlString: String): Try[EppType] = Try {
    val xmlElem = XML.loadString(xmlString)
    val child = xmlElem.child.collectFirst { case e: Elem => e }.getOrElse(
      throw new IllegalArgumentException("No child element in EPP XML")
    )
    val ns = Option(child.namespace).filter(_.nonEmpty)
    val record: DataRecord[Any] = child.label match {
      case "greeting"  => DataRecord(ns, Some("greeting"),  scalaxb.fromXML[GreetingType](child))
      case "response"  => DataRecord(ns, Some("response"),  scalaxb.fromXML[ResponseType](child))
      case "command"   => DataRecord(ns, Some("command"),   scalaxb.fromXML[CommandType](child))
      case "extension" => DataRecord(ns, Some("extension"), scalaxb.fromXML[ExtAnyType](child))
      case label       => throw new IllegalArgumentException(s"Unknown EPP element: $label")
    }
    EppType(record)
  }

  /**
   * Connect to an EPP server over TCP/TLS with RFC 5734 framing.
   *
   * Performs the TLS handshake and reads the initial server greeting.
   */
  def connect(host: String, port: Int, sslContext: SSLContext, readTimeoutMillis: Int = 60000)(implicit ec: ExecutionContext): EppTcpRpcServiceImpl = {
    val socket = sslContext.getSocketFactory.createSocket(host, port).asInstanceOf[SSLSocket]
    try {
      socket.setSoTimeout(readTimeoutMillis)
      socket.startHandshake()
      val input = new DataInputStream(socket.getInputStream)

      // Read the initial greeting frame
      val totalLength = input.readInt()
      if (totalLength < HeaderSize) {
        throw new IllegalArgumentException(s"Invalid EPP greeting frame: total length $totalLength < $HeaderSize")
      }
      val payloadLength = totalLength - HeaderSize
      val payload = new Array[Byte](payloadLength)
      input.readFully(payload)
      val greetingXml = new String(payload, "UTF-8")
      val greeting = parseEppType(greetingXml).get

      new EppTcpRpcServiceImpl(socket, greeting)
    } catch {
      case e: Exception =>
        Try(socket.close())
        throw e
    }
  }
}
