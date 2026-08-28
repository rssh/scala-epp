package ua.gradsoft.epp.rpc

import java.io.{DataOutputStream, EOFException, IOException, InputStream}
import java.net.SocketTimeoutException
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import javax.net.ssl.{SSLContext, SSLSocket}
import scala.concurrent.duration.{Deadline, DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future, TimeoutException, blocking}
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
 * @param requestTimeoutMillis Wall-clock bound on waiting for the connection, and again on the
 *                             exchange itself once it is held
 * @param executionContext The execution context for async operations
 */
class EppTcpRpcServiceImpl(
  val sslSocket: SSLSocket,
  val initialGreeting: EppType,
  val requestTimeoutMillis: Int = EppTcpRpcServiceImpl.DefaultRequestTimeoutMillis
)(implicit val executionContext: ExecutionContext) extends EppRpcServiceImpl {

  EppTcpRpcServiceImpl.requirePositiveBudget(requestTimeoutMillis)

  private val input: InputStream = sslSocket.getInputStream
  private val output = new DataOutputStream(sslSocket.getOutputStream)
  private val gate = new EppExchangeGate(requestTimeoutMillis)

  override def processEppMessage(request: EppType): Future[EppType] = {
    Future {
      blocking {
        gate.exchange { deadline =>
          writeFrame(serializeEppType(request))
          val responseBytes = readFrame(deadline)
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
    // The deadline does not reach the write: SO_TIMEOUT covers reads only, and nothing short of
    // closing the socket from another thread unblocks a stalled write. Commands are a few KB and
    // fit in the send buffer, so this stalls only against a peer that has stopped reading outright.
    // Through the logger, not stderr: level-controlled, and it lands in the configured appender
    // with the rest of the application rather than in the journal outside any rotation policy.
    eppLogger.debug(s"EPP >>> ${redact(xml)}")
    val payload = xml.getBytes("UTF-8")
    val totalLength = 4 + payload.length
    output.writeInt(totalLength)
    output.write(payload)
    output.flush()
  }

  private def readFrame(deadline: Deadline): Array[Byte] = {
    val payload =
      try EppTcpRpcServiceImpl.readFrame(input, deadline, sslSocket.setSoTimeout)
      catch {
        // Every one of these leaves the stream mid-frame: a late reply would be parsed as the next
        // response and desync every command after it. Close the socket so this connection is
        // discarded and the caller reconnects rather than reusing a stream it cannot trust.
        case e: SocketTimeoutException =>
          Try(sslSocket.close())
          val timeout = new SocketTimeoutException(
            s"No complete EPP response within ${requestTimeoutMillis}ms; connection closed"
          )
          timeout.initCause(e)
          throw timeout
        case e: IllegalArgumentException =>
          Try(sslSocket.close())
          throw new IllegalArgumentException(s"${e.getMessage}; connection closed", e)
        // A frame that ends early (EOF) or a reset connection: the message already carries what
        // arrived, so it is rethrown as it is - only the socket needs disposing of.
        case e: IOException =>
          Try(sslSocket.close())
          throw e
      }
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

  private[rpc] val DefaultRequestTimeoutMillis = 60000

  /**
   * 0 is SO_TIMEOUT's "block forever" - the value an operator reaching for "no timeout" picks, and
   * the one thing this bound exists to rule out. Rejecting it at startup beats accepting it and
   * turning every exchange into an instant failure that closes the socket after the command has
   * already gone out.
   */
  private[rpc] def requirePositiveBudget(millis: Int): Unit =
    require(millis > 0, s"EPP request timeout must be positive, got $millis")

  /**
   * Read one RFC 5734 frame, bounding the whole read - header and payload - by `deadline`.
   *
   * `setSoTimeout` is the socket's own per-read timeout, trimmed as the deadline approaches; it is
   * passed in rather than taken from a socket so the framing can be exercised over any stream.
   */
  private[rpc] def readFrame(in: InputStream, deadline: Deadline, setSoTimeout: Int => Unit): Array[Byte] = {
    val header = new Array[Byte](HeaderSize)
    readFullyByDeadline(in, header, deadline, setSoTimeout)
    val totalLength =
      ((header(0) & 0xff) << 24) | ((header(1) & 0xff) << 16) | ((header(2) & 0xff) << 8) | (header(3) & 0xff)
    if (totalLength < HeaderSize || totalLength > MaxFrameSize) {
      // A desynced stream reads arbitrary bytes as a length. Without an upper bound the
      // allocation below reaches ~2GB and takes the JVM down instead of just this connection.
      throw new IllegalArgumentException(
        s"Invalid EPP frame: total length $totalLength outside [$HeaderSize, $MaxFrameSize]"
      )
    }
    val payload = new Array[Byte](totalLength - HeaderSize)
    readFullyByDeadline(in, payload, deadline, setSoTimeout)
    payload
  }

  /**
   * Fill `buf`, treating `deadline` as a wall-clock bound on the whole fill.
   *
   * SO_TIMEOUT alone is an inter-byte timeout: a registry that drips one byte just before every
   * expiry resets it forever, so a plain readFully can block for hours on a frame that never
   * arrives. Trimming the timeout to what is left of the deadline before each read turns it into
   * the bound on total blocking that EppConfig.readTimeoutMillis promises.
   */
  private[rpc] def readFullyByDeadline(
    in: InputStream,
    buf: Array[Byte],
    deadline: Deadline,
    setSoTimeout: Int => Unit
  ): Unit = {
    var offset = 0
    while (offset < buf.length) {
      val leftMillis = deadline.timeLeft.toMillis
      if (leftMillis <= 0) {
        throw new SocketTimeoutException(
          s"EPP frame incomplete at the deadline: $offset of ${buf.length} bytes"
        )
      }
      // Never 0: that is SO_TIMEOUT's "block forever", the opposite of what is wanted here.
      setSoTimeout(math.min(leftMillis, Int.MaxValue.toLong).toInt)
      val read = in.read(buf, offset, buf.length - offset)
      if (read < 0) {
        throw new EOFException(s"EPP stream closed after $offset of ${buf.length} bytes")
      }
      offset += read
    }
  }

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
  def connect(host: String, port: Int, sslContext: SSLContext, readTimeoutMillis: Int = DefaultRequestTimeoutMillis)(implicit ec: ExecutionContext): EppTcpRpcServiceImpl = {
    requirePositiveBudget(readTimeoutMillis)
    val socket = sslContext.getSocketFactory.createSocket(host, port).asInstanceOf[SSLSocket]
    try {
      socket.setSoTimeout(readTimeoutMillis)
      socket.startHandshake()

      // The greeting is an ordinary frame and gets the same wall-clock bound as every later
      // response; the handshake above has only SO_TIMEOUT to lean on.
      val payload = readFrame(socket.getInputStream, readTimeoutMillis.millis.fromNow, socket.setSoTimeout)
      val greeting = parseEppType(new String(payload, "UTF-8")).get

      new EppTcpRpcServiceImpl(socket, greeting, readTimeoutMillis)
    } catch {
      case e: Exception =>
        Try(socket.close())
        throw e
    }
  }
}

/**
 * The connection, handed to one exchange at a time.
 *
 * The stream carries no request ids, so a second writer would interleave frames and every reader
 * after it would take someone else's answer. Bounding the wait for a turn matters as much as
 * bounding the reads: callers queue here on a dispatcher thread, so a queue behind one stalled
 * exchange pins as many threads as there are waiters.
 *
 * @param budgetMillis bound on waiting for a turn, and again on the exchange once it has one
 * @param stuckGrace how far past its own deadline a holder may be before it counts as stuck
 *                   rather than slow - it still has to unwind and close its socket
 */
private[rpc] final class EppExchangeGate(budgetMillis: Int, stuckGrace: FiniteDuration = 5.seconds) {

  private val lock = new ReentrantLock()

  /** Deadline of whoever holds the gate; None between exchanges. */
  @volatile private var holderDeadline: Option[Deadline] = None

  /**
   * Run `body` with the connection to itself, giving up if no turn comes within the budget.
   *
   * The exchange gets its own full budget rather than whatever the queue left over: a caller that
   * waited out most of one must not put a command on the wire and abandon it milliseconds later,
   * because the registry would go on to execute it while this side records a failure and closes
   * the socket. Total blocking is therefore bounded by twice the budget - once queueing, once on
   * the wire - and the write leg stays outside it, as writeFrame explains.
   */
  def exchange[T](body: Deadline => T): T = {
    if (!lock.tryLock(budgetMillis.toLong, TimeUnit.MILLISECONDS)) throw noTurnTaken()
    val deadline = budgetMillis.millis.fromNow
    holderDeadline = Some(deadline)
    try body(deadline)
    finally {
      holderDeadline = None
      lock.unlock()
    }
  }

  /**
   * A queue that never cleared says nothing about the socket by itself, and callers treat socket
   * errors as a reason to drop the session and re-login - which a merely busy connection does not
   * deserve. A holder long past its own deadline is the exception: it is stuck where the deadline
   * cannot reach, which in practice means a write to a peer that has stopped reading. Nothing here
   * can interrupt it, so the session has to go, and only a socket error asks callers to do that.
   */
  private def noTurnTaken(): Exception =
    holderDeadline.map(-_.timeLeft) match {
      case Some(overdueBy) if overdueBy > stuckGrace =>
        new SocketTimeoutException(
          s"EPP connection stuck: the exchange holding it is ${overdueBy.toSeconds}s past its ${budgetMillis}ms budget"
        )
      case _ =>
        new TimeoutException(s"EPP connection busy: no turn on it within ${budgetMillis}ms")
    }
}
