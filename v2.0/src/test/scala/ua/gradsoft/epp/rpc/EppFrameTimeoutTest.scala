package ua.gradsoft.epp.rpc

import java.io.{ByteArrayInputStream, EOFException, InputStream}
import java.net.SocketTimeoutException
import java.util.concurrent.CountDownLatch
import org.scalatest.funsuite.AnyFunSuite
import scala.concurrent.TimeoutException
import scala.concurrent.duration._

/**
 * The deadline is what makes readTimeoutMillis a bound on total blocking. SO_TIMEOUT on its own is
 * an inter-byte timeout, so these tests are mostly about a peer that keeps sending - slowly enough
 * that it would hold the connection, and the dispatcher thread on it, for hours.
 */
class EppFrameTimeoutTest extends AnyFunSuite {

  /** Serves one byte per read after `gapMillis`, the way a registry that never finishes a frame would. */
  private class DrippingStream(totalBytes: Int, gapMillis: Long) extends InputStream {
    private var served = 0

    override def read(): Int = {
      val one = new Array[Byte](1)
      if (read(one, 0, 1) < 0) -1 else one(0) & 0xff
    }

    override def read(buf: Array[Byte], off: Int, len: Int): Int = {
      if (served >= totalBytes) return -1
      Thread.sleep(gapMillis)
      served += 1
      buf(off) = 'A'.toByte
      1
    }
  }

  private def frameOf(payload: Array[Byte]): Array[Byte] = {
    val total = 4 + payload.length
    Array[Byte]((total >>> 24).toByte, (total >>> 16).toByte, (total >>> 8).toByte, total.toByte) ++ payload
  }

  private def millisSince(startNanos: Long): Long = (System.nanoTime() - startNanos) / 1000000L

  test("a dripping stream is abandoned at the deadline, not followed to the end of the frame") {
    val buf = new Array[Byte](1000) // at 20ms a byte, reading it all would take 20 seconds
    val stream = new DrippingStream(buf.length, 20)

    val startNanos = System.nanoTime()
    val thrown = intercept[SocketTimeoutException] {
      EppTcpRpcServiceImpl.readFullyByDeadline(stream, buf, 200.millis.fromNow, _ => ())
    }
    val elapsed = millisSince(startNanos)

    assert(elapsed < 5000, s"read ran for ${elapsed}ms: the deadline did not bound it")
    assert(thrown.getMessage.contains("of 1000 bytes"), thrown.getMessage)
  }

  test("the per-read timeout shrinks with the deadline and is never set to block forever") {
    val timeouts = scala.collection.mutable.ArrayBuffer.empty[Int]
    val buf = new Array[Byte](50)
    val stream = new DrippingStream(buf.length, 10)

    intercept[SocketTimeoutException] {
      EppTcpRpcServiceImpl.readFullyByDeadline(stream, buf, 100.millis.fromNow, t => { timeouts += t; () })
    }

    assert(timeouts.nonEmpty, "no per-read timeout was applied")
    assert(timeouts.forall(t => t > 0 && t <= 100), s"outside the budget: $timeouts")
    assert(timeouts == timeouts.sorted(using Ordering[Int].reverse), s"not shrinking towards the deadline: $timeouts")
  }

  test("a complete frame is read as its payload") {
    val payload = EppTcpRpcServiceImpl.readFrame(
      new ByteArrayInputStream(frameOf("<epp/>".getBytes("UTF-8"))),
      5.seconds.fromNow,
      _ => ()
    )
    assert(new String(payload, "UTF-8") == "<epp/>")
  }

  test("a length past the frame limit is rejected before the payload is allocated") {
    val header = Array[Byte](0x7f, -1, -1, -1) // 2147483647: a desynced stream read as a length
    val thrown = intercept[IllegalArgumentException] {
      EppTcpRpcServiceImpl.readFrame(new ByteArrayInputStream(header), 5.seconds.fromNow, _ => ())
    }
    assert(thrown.getMessage.contains("2147483647"), thrown.getMessage)
  }

  test("a stream that ends mid-frame reports how much of the payload arrived") {
    val truncated = frameOf("<epp/>".getBytes("UTF-8")).take(7)
    val thrown = intercept[EOFException] {
      EppTcpRpcServiceImpl.readFrame(new ByteArrayInputStream(truncated), 5.seconds.fromNow, _ => ())
    }
    assert(thrown.getMessage.contains("3 of 6 bytes"), thrown.getMessage)
  }

  /** Holds the gate until `release`, so a waiter can be observed giving up. */
  private def holdingGate(gate: EppExchangeGate, release: CountDownLatch): Thread = {
    val held = new CountDownLatch(1)
    val holder = new Thread(() => {
      try gate.exchange { _ => held.countDown(); release.await() }
      catch { case _: Throwable => () }
    })
    holder.setDaemon(true)
    holder.start()
    held.await()
    holder
  }

  test("waiting for a busy connection ends at the deadline, without condemning the session") {
    val release = new CountDownLatch(1)
    val gate = new EppExchangeGate(budgetMillis = 200)
    val holder = holdingGate(gate, release)

    try {
      val startNanos = System.nanoTime()
      val thrown = intercept[TimeoutException] {
        gate.exchange(_ => fail("the exchange must not run"))
      }
      val elapsed = millisSince(startNanos)

      assert(elapsed < 5000, s"waited ${elapsed}ms for a turn it was never going to get")
      assert(thrown.getMessage.contains("busy"), thrown.getMessage)
      // A busy queue is not a broken socket: callers reconnect on SocketTimeoutException, and
      // tearing down a working session over a queue is exactly what this type keeps them from.
      assert(!thrown.isInstanceOf[SocketTimeoutException])
    } finally {
      release.countDown()
      holder.join()
    }
  }

  test("a holder stuck past its own budget is reported as a socket failure, so the session is dropped") {
    val release = new CountDownLatch(1)
    // The holder is wedged where no deadline reaches - a write to a peer that stopped reading.
    val gate = new EppExchangeGate(budgetMillis = 100, stuckGrace = 100.millis)
    val holder = holdingGate(gate, release)

    try {
      Thread.sleep(250) // the holder is now past its budget and the grace on top of it
      val thrown = intercept[SocketTimeoutException] {
        gate.exchange(_ => fail("the exchange must not run"))
      }
      assert(thrown.getMessage.contains("stuck"), thrown.getMessage)
    } finally {
      release.countDown()
      holder.join()
    }
  }

  test("an exchange gets a full budget of its own, not what the queue left over") {
    val release = new CountDownLatch(1)
    val gate = new EppExchangeGate(budgetMillis = 500)
    val holder = holdingGate(gate, release)
    new Thread(() => { Thread.sleep(300); release.countDown() }).start()

    // Waiting out most of the budget must not leave the winner with a sliver: it would put a
    // command on the wire, abandon it, and close a connection the registry is still answering on.
    val leftMillis = gate.exchange(deadline => deadline.timeLeft.toMillis)
    assert(leftMillis > 400, s"the exchange started with only ${leftMillis}ms of its 500ms budget")
    holder.join()
  }

  test("a failed exchange still hands the connection back") {
    val gate = new EppExchangeGate(budgetMillis = 1000)
    intercept[RuntimeException] {
      gate.exchange(_ => throw new RuntimeException("boom"))
    }
    // A second exchange can only start if the first released the gate.
    assert(gate.exchange(_ => "free") == "free")
  }

  test("a budget of zero is refused rather than read as SO_TIMEOUT's block-forever") {
    val thrown = intercept[IllegalArgumentException] {
      EppTcpRpcServiceImpl.requirePositiveBudget(0)
    }
    assert(thrown.getMessage.contains("must be positive"), thrown.getMessage)
  }
}
