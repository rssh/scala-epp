package ua.gradsoft.epp.rpc

import org.scalatest.funsuite.AnyFunSuite
import ua.gradsoft.epp.xsdmodel._
import scala.concurrent.{Await, ExecutionContext}
import scala.concurrent.duration._

class MockEppRpcServiceTest extends AnyFunSuite {

  implicit val ec: ExecutionContext = ExecutionContext.global

  test("hello() returns a valid GreetingType") {
    val mockService = new MockEppRpcService()
    val greetingFuture = mockService.hello()
    val greeting = Await.result(greetingFuture, 5.seconds)

    assert(greeting.svID == "EPP Mock Server")
    assert(greeting.svcMenu.version.contains(Number1u460))
    assert(greeting.svcMenu.lang.contains("en"))
    assert(greeting.svcMenu.lang.contains("uk"))
    assert(greeting.svcMenu.objURI.exists(_.toString.contains("domain")))
    assert(greeting.svcMenu.objURI.exists(_.toString.contains("contact")))
    assert(greeting.svcMenu.objURI.exists(_.toString.contains("host")))
  }

  test("login() returns a valid GreetingType") {
    val mockService = new MockEppRpcService()

    // Create a minimal LoginType
    val loginType = LoginType(
      clID = "testuser",
      pw = "testpassword",
      newPW = None,
      options = CredsOptionsType(
        version = Number1u460,
        lang = "en"
      ),
      svcs = LoginSvcType(
        objURI = Seq(new java.net.URI("urn:ietf:params:xml:ns:domain-1.0")),
        svcExtension = None
      )
    )

    val greetingFuture = mockService.login(loginType)
    val greeting = Await.result(greetingFuture, 5.seconds)

    assert(greeting.svID == "EPP Mock Server - Authenticated")
    assert(greeting.svcMenu.version.contains(Number1u460))
  }

  test("logout() returns a valid ResponseType with code 1500") {
    val mockService = new MockEppRpcService()
    val responseFuture = mockService.logout()
    val response = Await.result(responseFuture, 5.seconds)

    assert(response.result.nonEmpty)
    assert(response.result.head.code == Number1500)
    assert(response.result.head.msg.value.contains("ending session"))
    assert(response.trID.svTRID.startsWith("MOCK-"))
  }

  test("domainCheck() marks .available domains as available") {
    val mockService = new MockEppRpcService()

    val checkType = MNameTypeType(
      name = Seq("test.available", "example.available")
    )

    val checkResultFuture = mockService.domainCheck(checkType)
    val checkResult = Await.result(checkResultFuture, 5.seconds)

    assert(checkResult.cd.length == 2)

    val firstDomain = checkResult.cd.head
    assert(firstDomain.name.value == "test.available")
    assert(firstDomain.name.avail == true)
    assert(firstDomain.reason.isEmpty)

    val secondDomain = checkResult.cd(1)
    assert(secondDomain.name.value == "example.available")
    assert(secondDomain.name.avail == true)
    assert(secondDomain.reason.isEmpty)
  }

  test("domainCheck() marks .taken domains as unavailable with 'Object exists' reason") {
    val mockService = new MockEppRpcService()

    val checkType = MNameTypeType(
      name = Seq("registered.taken")
    )

    val checkResultFuture = mockService.domainCheck(checkType)
    val checkResult = Await.result(checkResultFuture, 5.seconds)

    assert(checkResult.cd.length == 1)

    val domain = checkResult.cd.head
    assert(domain.name.value == "registered.taken")
    assert(domain.name.avail == false)
    assert(domain.reason.isDefined)
    assert(domain.reason.get.value == "Object exists")
  }

  test("domainCheck() marks other domains as unavailable with 'Incorrect domain name' reason") {
    val mockService = new MockEppRpcService()

    val checkType = MNameTypeType(
      name = Seq("example.com", "test.org")
    )

    val checkResultFuture = mockService.domainCheck(checkType)
    val checkResult = Await.result(checkResultFuture, 5.seconds)

    assert(checkResult.cd.length == 2)

    checkResult.cd.foreach { domain =>
      assert(domain.name.avail == false)
      assert(domain.reason.isDefined)
      assert(domain.reason.get.value == "Incorrect domain name")
    }
  }

  test("domainCheck() handles mixed domain statuses") {
    val mockService = new MockEppRpcService()

    val checkType = MNameTypeType(
      name = Seq("free.available", "owned.taken", "invalid.xyz")
    )

    val checkResultFuture = mockService.domainCheck(checkType)
    val checkResult = Await.result(checkResultFuture, 5.seconds)

    assert(checkResult.cd.length == 3)

    // First domain: available
    assert(checkResult.cd(0).name.value == "free.available")
    assert(checkResult.cd(0).name.avail == true)
    assert(checkResult.cd(0).reason.isEmpty)

    // Second domain: taken
    assert(checkResult.cd(1).name.value == "owned.taken")
    assert(checkResult.cd(1).name.avail == false)
    assert(checkResult.cd(1).reason.get.value == "Object exists")

    // Third domain: invalid
    assert(checkResult.cd(2).name.value == "invalid.xyz")
    assert(checkResult.cd(2).name.avail == false)
    assert(checkResult.cd(2).reason.get.value == "Incorrect domain name")
  }

  test("unimplemented methods throw UnsupportedOperationException") {
    val mockService = new MockEppRpcService()

    // Test that unimplemented methods fail with appropriate exception
    val command = CommandType(
      commandtypeoption = scalaxb.DataRecord(None, Some("check"), ReadWriteType(scalaxb.DataRecord(None, None, "")))
    )

    val executeFuture = mockService.executeCommand(command)

    assertThrows[UnsupportedOperationException] {
      Await.result(executeFuture, 5.seconds)
    }
  }
}
