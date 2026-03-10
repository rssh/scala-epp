package ua.gradsoft.epp.rpc

import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers
import ua.gradsoft.epp.xsdmodel._

import scala.concurrent.Future
import scala.xml.XML

/**
 * Tests that real EPP XML responses from the server can be deserialized
 * through the EppRpcServiceImpl pipeline (scalaxb.fromXML -> extractEppContent/extractResponseData).
 *
 * This catches issues like ClassCastException when DataRecord holds raw Elem instead of deserialized types.
 */
class EppXmlDeserializationTest extends AsyncFunSpec with Matchers {

  /**
   * A test implementation of EppRpcServiceImpl that returns a pre-parsed EppType from XML string,
   * simulating what EppTcpRpcServiceImpl.deserializeEppType does.
   */
  class XmlEppRpcService(responseXml: String) extends EppRpcServiceImpl {
    implicit val executionContext = scala.concurrent.ExecutionContext.global

    override def processEppMessage(input: EppType): Future[EppType] = {
      Future.successful {
        val xmlElem = XML.loadString(responseXml)
        scalaxb.fromXML[EppType](xmlElem)
      }
    }
  }

  describe("EPP XML deserialization through EppRpcServiceImpl") {

    it("should deserialize a greeting response") {
      val greetingXml = """<?xml version="1.0" encoding="UTF-8"?>
        |<epp xmlns="urn:ietf:params:xml:ns:epp-1.0">
        |  <greeting>
        |    <svID>UANIC EPP Server version 0.6.0</svID>
        |    <svDate>2010-06-04T10:34:40.617+03:00</svDate>
        |    <svcMenu>
        |      <version>1.0</version>
        |      <lang>en</lang>
        |      <lang>uk</lang>
        |      <objURI>urn:ietf:params:xml:ns:epp-1.0</objURI>
        |      <objURI>http://hostmaster.ua/epp/contact-1.1</objURI>
        |      <objURI>http://hostmaster.ua/epp/domain-1.1</objURI>
        |      <objURI>http://hostmaster.ua/epp/host-1.1</objURI>
        |      <svcExtension>
        |        <extURI>http://hostmaster.ua/epp/rgp-1.1</extURI>
        |        <extURI>http://hostmaster.ua/epp/uaepp-1.1</extURI>
        |      </svcExtension>
        |    </svcMenu>
        |    <dcp>
        |      <access><all/></access>
        |      <statement>
        |        <purpose><admin/><prov/></purpose>
        |        <recipient><public/></recipient>
        |        <retention><stated/></retention>
        |      </statement>
        |    </dcp>
        |  </greeting>
        |</epp>""".stripMargin

      val service = new XmlEppRpcService(greetingXml)
      service.hello().map { greeting =>
        greeting.svID should be("UANIC EPP Server version 0.6.0")
        greeting.svcMenu.lang should contain("en")
        greeting.svcMenu.lang should contain("uk")
        greeting.svcMenu.objURI.map(_.toString) should contain("http://hostmaster.ua/epp/domain-1.1")
      }
    }

    it("should deserialize a login response") {
      val loginResponseXml = """<?xml version="1.0" encoding="UTF-8"?>
        |<epp xmlns="urn:ietf:params:xml:ns:epp-1.0">
        |  <response>
        |    <result code="1000">
        |      <msg lang="en">Command completed successfully</msg>
        |    </result>
        |    <trID>
        |      <clTRID>USER-1275641748</clTRID>
        |      <svTRID>UA-20100604115549-179862-00001</svTRID>
        |    </trID>
        |  </response>
        |</epp>""".stripMargin

      val service = new XmlEppRpcService(loginResponseXml)
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
      service.login(loginType).map { response =>
        response.result should have size 1
        response.result.head.code should be(Number1000)
        response.result.head.msg.value should be("Command completed successfully")
        response.trID.clTRID should be(Some("USER-1275641748"))
        response.trID.svTRID should be("UA-20100604115549-179862-00001")
      }
    }

    it("should deserialize a domain create response with resData") {
      val domainCreateResponseXml = """<?xml version="1.0" encoding="UTF-8"?>
        |<epp xmlns="urn:ietf:params:xml:ns:epp-1.0">
        |  <response>
        |    <result code="1000">
        |      <msg lang="en">Command completed successfully</msg>
        |    </result>
        |    <resData>
        |      <domain:creData xmlns:domain="http://hostmaster.ua/epp/domain-1.1">
        |        <domain:name>example1.epp1.ua</domain:name>
        |        <domain:crDate>2010-06-10T15:03:12+03:00</domain:crDate>
        |        <domain:exDate>2012-06-10T15:03:12+03:00</domain:exDate>
        |      </domain:creData>
        |    </resData>
        |    <trID>
        |      <clTRID>USER-1276171392</clTRID>
        |      <svTRID>UA-20100610150312-508777-00002</svTRID>
        |    </trID>
        |  </response>
        |</epp>""".stripMargin

      val service = new XmlEppRpcService(domainCreateResponseXml)
      val createType = CreateTypeType2(
        name = "example1.epp1.ua",
        registrant = "testregistrant"
      )
      service.domainCreate(createType).map { creData =>
        creData.name should be("example1.epp1.ua")
      }
    }
  }
}
