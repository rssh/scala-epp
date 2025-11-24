package ua.gradsoft.epp.rpc

import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers
import ua.gradsoft.epp.xsdmodel._

import scala.concurrent.Future

class ContactUpdateTest extends AsyncFunSpec with Matchers {

  describe("MockEppRpcService contactUpdate") {

    it("should return success for a valid contact update request") {
      val service = new MockEppRpcService
      val contactId = "sh8013"
      val updateType = UpdateTypeType(
        id = contactId,
        chg = Some(ChgType(
          email = Some("new.email@example.com")
        ))
      )

      val futureResult: Future[ResponseType] = service.contactUpdate(updateType)

      futureResult.map { result =>
        result.result.head.code should be(Number1000)
      }
    }

    it("should return an error for a non-existing contact") {
      val service = new MockEppRpcService
      val contactId = "sh8014"
      val updateType = UpdateTypeType(
        id = contactId,
        chg = Some(ChgType(
          email = Some("new.email@example.com")
        ))
      )

      val futureResult: Future[ResponseType] = service.contactUpdate(updateType)

      recoverToSucceededIf[EppErrorException] {
        futureResult
      }
    }
  }
}
