package ua.gradsoft.epp.test

import org.scalatest.funsuite.AnyFunSuite
import _root_.ua.gradsoft.epp.xsdmodel._

class DomainTest extends AnyFunSuite {

  test("Create a simple domain create object") {
    val create = CreateTypeType2(
      name = "example.com",
      registrant = "client1",
      ns = Some(NsType(
        nstypeoption = Seq(
          scalaxb.DataRecord(None, Some("hostObj"), "ns1.example.com"),
          scalaxb.DataRecord(None, Some("hostObj"), "ns2.example.com")
        )
      ))
    )
    assert(create.name == "example.com")
  }

}
