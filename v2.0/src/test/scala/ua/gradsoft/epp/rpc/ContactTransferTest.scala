package ua.gradsoft.epp.rpc

import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers
import ua.gradsoft.epp.xsdmodel._

import scala.concurrent.Future

class ContactTransferTest extends AsyncFunSpec with Matchers {

  describe("MockEppRpcService contactTransfer") {

    it("should return success for a valid contact transfer request") {
      val service = new MockEppRpcService
      val contactId = "sh8013"
      val transferType = AuthIDType(
        id = contactId
      )

      val futureResult: Future[TrnDataType] = service.contactTransfer(transferType)

      futureResult.map { result =>
        result.id should be(contactId)
        result.trStatus should be(ServerApproved)
      }
    }

    it("should return an error for a non-existing contact") {
      val service = new MockEppRpcService
      val contactId = "sh8014"
      val transferType = AuthIDType(
        id = contactId
      )

      val futureResult: Future[TrnDataType] = service.contactTransfer(transferType)

      recoverToSucceededIf[EppErrorException] {
        futureResult
      }
    }
  }
}
