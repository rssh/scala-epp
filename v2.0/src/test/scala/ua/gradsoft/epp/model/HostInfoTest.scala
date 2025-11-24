package ua.gradsoft.epp.model

import org.scalatest.funsuite.AnyFunSuite
import ua.gradsoft.epp.xsdmodel._
import ua.gradsoft.epp.xsdmodel.{`package` => eppXsdModel}
import eppXsdModel.given
import javax.xml.datatype.DatatypeFactory
import java.time.Instant

class HostInfoTest extends AnyFunSuite {

  test("Create HostInfo from InfDataType") {
    val crDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2023-01-15T10:30:00Z")
    val upDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2023-02-20T11:00:00Z")
    val trDateXml = DatatypeFactory.newInstance().newXMLGregorianCalendar("2023-03-25T12:00:00Z")

    val infData = InfDataType(
      name = "ns1.example.com",
      roid = "NS1-ROID",
      status = Seq(
        StatusType(value = "ok", attributes = Map("@s" -> scalaxb.DataRecord[StatusValueType](Ok)))
      ),
      addr = Seq(
        AddrType(value = "192.0.2.1", attributes = Map("@ip" -> scalaxb.DataRecord[IpType](V4))),
        AddrType(value = "2001:db8::1", attributes = Map("@ip" -> scalaxb.DataRecord[IpType](V6)))
      ),
      clID = "client1",
      crID = "client1",
      crDate = crDateXml,
      upID = Some("client1"),
      upDate = Some(upDateXml),
      trDate = Some(trDateXml)
    )

    val hostInfo = HostInfo.fromInfDataType(infData)

    assert(hostInfo.name == "ns1.example.com")
    assert(hostInfo.roid == "NS1-ROID")
    assert(hostInfo.status.head.status == HostStatus.Ok)
    assert(hostInfo.status.head.message.contains("ok"))
    assert(hostInfo.addresses.size == 2)
    assert(hostInfo.addresses("192.0.2.1") == V4)
    assert(hostInfo.addresses("2001:db8::1") == V6)
    assert(hostInfo.sponsoringClientID == "client1")
    assert(hostInfo.creatingClientID == "client1")
    assert(hostInfo.creationDate == Instant.parse("2023-01-15T10:30:00Z"))
    assert(hostInfo.lastUpdateClientID.contains("client1"))
    assert(hostInfo.lastUpdateDate.contains(Instant.parse("2023-02-20T11:00:00Z")))
    assert(hostInfo.lastTransferDate.contains(Instant.parse("2023-03-25T12:00:00Z")))
  }

}
