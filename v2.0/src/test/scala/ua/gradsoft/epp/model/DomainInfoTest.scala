package ua.gradsoft.epp.model

import org.scalatest.funsuite.AnyFunSuite
import ua.gradsoft.epp.xsdmodel._
import ua.gradsoft.epp.xsdmodel.{`package` => eppXsdModel}
import eppXsdModel.given
import javax.xml.datatype.DatatypeFactory
import java.time.Instant
import ua.gradsoft.epp.xsdmodel.{ContactType => XsdContactType}
import ua.gradsoft.epp.model.ContactType

class DomainInfoTest extends AnyFunSuite {

  test("Create DomainInfo from InfDataTypeType2") {
    val crDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2023-01-15T10:30:00Z")
    val upDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2023-02-20T11:00:00Z")
    val trDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2023-03-25T12:00:00Z")
    val exDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2024-01-15T10:30:00Z")

    val infData = InfDataTypeType2(
      name = "example.com",
      roid = "EXAMPLE-ROID",
      status = Seq(
        StatusTypeType2(value = "ok", attributes = Map("@s" -> scalaxb.DataRecord[StatusValueTypeType2](OkValue2)))
      ),
      registrant = Some("client1"),
      clID = "client1",
      crID = Some("client1"),
      upID = Some("client1"),
      ns = Some(NsType(
        nstypeoption = Seq(
          scalaxb.DataRecord(None, Some("hostObj"), "ns1.example.com"),
          scalaxb.DataRecord(None, Some("hostObj"), "ns2.example.com")
        )
      )),
      host = Seq("host1.example.com", "host2.example.com"),
      contact = Seq(
        XsdContactType(value = "contact-admin", attributes = Map("@type" -> scalaxb.DataRecord[ContactAttrType](Admin))),
        XsdContactType(value = "contact-tech", attributes = Map("@type" -> scalaxb.DataRecord[ContactAttrType](Tech)))
      ),
      crDate = Some(crDateXml),
      upDate = Some(upDateXml),
      trDate = Some(trDateXml),
      exDate = Some(exDateXml),
      license = Some("license123"),
      authInfo = Some(AuthInfoTypeType(scalaxb.DataRecord(None, Some("pw"), PwAuthInfoType("password123"))))
    )
    val domainInfo = DomainInfo.fromInfDataType(infData)
    assert(domainInfo.name == "example.com")
    assert(domainInfo.roid == "EXAMPLE-ROID")
    assert(domainInfo.status.head.status == DomainStatus.Ok)
    assert(domainInfo.status.head.message.contains("ok"))
    assert(domainInfo.registrant.contains("client1"))
    assert(domainInfo.sponsoringClientID == "client1")
    assert(domainInfo.creatingClientID.contains("client1"))
    assert(domainInfo.lastUpdateClientID.contains("client1"))
    assert(domainInfo.nameservers == Seq("ns1.example.com", "ns2.example.com"))
    assert(domainInfo.hosts == Seq("host1.example.com", "host2.example.com"))
    assert(domainInfo.contacts.size == 2)
    assert(domainInfo.contacts.head.id == "contact-admin")
    assert(domainInfo.contacts.head.contactType.contains(ContactType.Admin))
    assert(domainInfo.contacts(1).id == "contact-tech")
    assert(domainInfo.contacts(1).contactType.contains(ContactType.Tech))
    assert(domainInfo.creationDate.contains(Instant.parse("2023-01-15T10:30:00Z")))
    assert(domainInfo.lastUpdateDate.contains(Instant.parse("2023-02-20T11:00:00Z")))
    assert(domainInfo.lastTransferDate.contains(Instant.parse("2023-03-25T12:00:00Z")))
    assert(domainInfo.expirationDate.contains(Instant.parse("2024-01-15T10:30:00Z")))
    assert(domainInfo.license.contains("license123"))
    assert(domainInfo.authInfo.contains("password123"))
  }

}
