package ua.gradsoft.epp.model

import org.scalatest.funsuite.AnyFunSuite
import ua.gradsoft.epp.xsdmodel._
import ua.gradsoft.epp.xsdmodel.{`package` => eppXsdModel}
import eppXsdModel.given
import javax.xml.datatype.DatatypeFactory
import java.time.Instant

class ContactInfoTest extends AnyFunSuite {

  test("Create ContactInfo from InfDataTypeType") {
    val crDate = DatatypeFactory.newInstance().newXMLGregorianCalendar("2025-11-16T13:00:00Z")
    val upDate = DatatypeFactory.newInstance().newXMLGregorianCalendar("2025-11-16T14:00:00Z")
    val trDate = DatatypeFactory.newInstance().newXMLGregorianCalendar("2025-11-16T15:00:00Z")
    val infData = InfDataTypeType(
      id = "contact1",
      roid = "CONTACT-ROID",
      email = "test@example.com",
      clID = "client1",
      crID = "client1",
      crDate = crDate,
      upID = Some("client1"),
      upDate = Some(upDate),
      trDate = Some(trDate)
    )
    val contactInfo = ContactInfo.fromInfDataType(infData)
    assert(contactInfo.id == "contact1")
    assert(contactInfo.roid == "CONTACT-ROID")
    assert(contactInfo.email == "test@example.com")
    assert(contactInfo.sponsoringClientID == "client1")
    assert(contactInfo.creatingClientID == "client1")
    assert(contactInfo.lastUpdateClientID.contains("client1"))
    assert(contactInfo.creationDate == Instant.parse("2025-11-16T13:00:00Z"))
    assert(contactInfo.lastUpdateDate.contains(Instant.parse("2025-11-16T14:00:00Z")))
    assert(contactInfo.lastTransferDate.contains(Instant.parse("2025-11-16T15:00:00Z")))
  }

}
