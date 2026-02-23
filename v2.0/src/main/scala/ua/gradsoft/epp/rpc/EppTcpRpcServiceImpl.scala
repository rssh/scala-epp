package ua.gradsoft.epp.rpc

import java.io.{DataInputStream, DataOutputStream}
import javax.net.ssl.{SSLContext, SSLSocket}
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.{Failure, Success, Try}
import scala.xml.{Elem, XML}
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
          deserializeEppType(new String(responseBytes, "UTF-8")) match {
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

  private def writeFrame(xml: String): Unit = {
    val payload = xml.getBytes("UTF-8")
    val totalLength = 4 + payload.length
    output.writeInt(totalLength)
    output.write(payload)
    output.flush()
  }

  private def readFrame(): Array[Byte] = {
    val totalLength = input.readInt()
    if (totalLength < 4) {
      throw new IllegalArgumentException(s"Invalid EPP frame: total length $totalLength < 4")
    }
    val payloadLength = totalLength - 4
    val payload = new Array[Byte](payloadLength)
    input.readFully(payload)
    payload
  }

  private def serializeEppType(eppType: EppType): String = {
    val xmlNodes = scalaxb.toXML[EppType](eppType, Some("urn:ietf:params:xml:ns:epp-1.0"), Some("epp"), defaultScope)
    val xmlElem = xmlNodes.head.asInstanceOf[Elem]
    """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + xmlElem.toString()
  }

  private def deserializeEppType(xmlString: String): Try[EppType] = {
    Try {
      val xmlElem = XML.loadString(xmlString)
      scalaxb.fromXML[EppType](xmlElem)
    }
  }
}

object EppTcpRpcServiceImpl {

  private val HeaderSize = 4

  /**
   * Connect to an EPP server over TCP/TLS with RFC 5734 framing.
   *
   * Performs the TLS handshake and reads the initial server greeting.
   */
  def connect(host: String, port: Int, sslContext: SSLContext)(implicit ec: ExecutionContext): EppTcpRpcServiceImpl = {
    val socket = sslContext.getSocketFactory.createSocket(host, port).asInstanceOf[SSLSocket]
    try {
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
      val greeting = scalaxb.fromXML[EppType](XML.loadString(greetingXml))

      new EppTcpRpcServiceImpl(socket, greeting)
    } catch {
      case e: Exception =>
        Try(socket.close())
        throw e
    }
  }
}
