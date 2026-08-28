package ua.gradsoft.epp

import org.scalatest.funsuite.AnyFunSuite
import ua.gradsoft.epp.model.DomainInfo
import ua.gradsoft.epp.rpc.{EppErrorException, MockEppRpcService}
import ua.gradsoft.epp.xsdmodel._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * .ua needs a host object to exist before a domain can name it, so createDomain/updateDomain make
 * the missing ones. These cover what happens when the domain command that asked for them does not
 * go through, and what a caller is told when a host cannot be made at all.
 */
class HostProvisioningTest extends AnyFunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  /**
   * Records what reached the registry. Host creates and deletes fall through to the mock, whose
   * `.com` names succeed; domain creates follow its `.available` / `.taken` suffixes.
   */
  private class RecordingRpc(
      present: Set[String] = Set.empty,
      refuse: Map[String, EppErrorException] = Map.empty
  ) extends MockEppRpcService {

    val created: ArrayBuffer[String] = ArrayBuffer.empty
    val deleted: ArrayBuffer[String] = ArrayBuffer.empty

    override def hostCheck(checkType: MNameType): Future[ChkDataType] =
      Future.successful(ChkDataType(cd = checkType.name.map { name =>
        CheckType(
          name = CheckNameType(
            value = name,
            attributes = Map("@avail" -> scalaxb.DataRecord(!present.contains(name) && !created.contains(name)))
          ),
          reason = None
        )
      }))

    override def hostCreate(createType: CreateType): Future[CreDataType] =
      refuse.get(createType.name) match {
        case Some(error) => Future.failed(error)
        case None =>
          created += createType.name
          super.hostCreate(createType)
      }

    override def hostDelete(deleteType: SNameType): Future[ResponseType] = {
      deleted += deleteType.name
      super.hostDelete(deleteType)
    }
  }

  private def domain(name: String, nameservers: String*): DomainInfo =
    DomainInfo(
      name = name, roid = "", status = Seq.empty, registrant = Some("REG-1"), contacts = Seq.empty,
      nameservers = nameservers, hosts = Seq.empty, sponsoringClientID = "ua.gradsoft",
      creatingClientID = None, creationDate = None, lastUpdateClientID = None, lastUpdateDate = None,
      expirationDate = None, lastTransferDate = None, license = None, authInfo = Some("pw")
    )

  private def await[T](f: Future[T]): T = Await.result(f, 5.seconds)

  test("a nameserver the registry does not have is created before the domain that uses it") {
    val rpc = new RecordingRpc()
    val connection = new EppConnectionImpl(rpc)

    await(connection.createDomain(domain("gradsoft.available", "ns1.example.com"), None, None))

    assertResult(Seq("ns1.example.com"))(rpc.created.toSeq)
    assert(rpc.deleted.isEmpty, "nothing failed, so nothing should have been taken back")
  }

  test("a nameserver the registry already has is left alone") {
    val rpc = new RecordingRpc(present = Set("ns1.example.com"))
    val connection = new EppConnectionImpl(rpc)

    await(connection.createDomain(domain("gradsoft.available", "ns1.example.com"), None, None))

    assert(rpc.created.isEmpty, "the host was already there")
  }

  test("hosts created for a domain that could not be created are taken back out") {
    // Without this the host object survives a domain create that never happened: nothing in the
    // portal refers to it, and only someone at the registry can clean it up.
    val rpc = new RecordingRpc()
    val connection = new EppConnectionImpl(rpc)

    val failure = intercept[EppErrorException] {
      await(connection.createDomain(domain("gradsoft.taken", "ns1.example.com", "ns2.example.com"), None, None))
    }

    assertResult(Number2302)(failure.code) // the domain failure reaches the caller, not a cleanup one
    assertResult(Seq("ns1.example.com", "ns2.example.com"))(rpc.created.toSeq)
    assertResult(rpc.created.toSeq)(rpc.deleted.toSeq)
  }

  test("a nameserver that was already registered is not removed when the domain fails") {
    val rpc = new RecordingRpc(present = Set("ns1.example.com"))
    val connection = new EppConnectionImpl(rpc)

    intercept[EppErrorException] {
      await(connection.createDomain(domain("gradsoft.taken", "ns1.example.com"), None, None))
    }

    assert(rpc.deleted.isEmpty, "a host this call did not create is not this call's to remove")
  }

  test("a refused host create names the domain and, inside .ua, says glue is what is missing") {
    // 2306 against a name the caller never typed is what the registry gives back for an in-zone
    // host with no addresses; on its own it reads as an error about some unrelated object.
    val rpc = new RecordingRpc(
      refuse = Map("ns1.gradsoft.com.ua" -> EppErrorException("Parameter value policy error", Number2306))
    )
    val connection = new EppConnectionImpl(rpc)

    val failure = intercept[EppErrorException] {
      await(connection.createDomain(domain("gradsoft.available", "ns0.example.com", "ns1.gradsoft.com.ua"), None, None))
    }

    assertResult(Number2306)(failure.code) // callers route on the code, so it has to survive
    assert(failure.msg.contains("gradsoft.available"), failure.msg)
    assert(failure.msg.contains("glue"), failure.msg)
    // The host made before the refused one does not outlive the create it was made for.
    assertResult(Seq("ns0.example.com"))(rpc.deleted.toSeq)
  }
}
