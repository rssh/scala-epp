package ua.gradsoft.epp.rpc

import scala.concurrent.{Future, ExecutionContext}
import ua.gradsoft.epp.rpc.EppErrorException
import ua.gradsoft.epp.xsdmodel._
import javax.xml.datatype.{DatatypeFactory, XMLGregorianCalendar}
import java.time.Instant
import java.util.GregorianCalendar

/**
 * Mock implementation of EppRpcService for testing purposes.
 *
 * This mock implements the EPP protocol responses based on the specifications from:
 * - https://epp.hostmaster.ua/help/commands/?login
 * - https://epp.hostmaster.ua/help/commands/?dchk
 */
class MockEppRpcService(implicit val executionContext: ExecutionContext) extends EppRpcService {

  private val datatypeFactory = DatatypeFactory.newInstance()

  // Helper to create XMLGregorianCalendar from current time
  private def createXMLGregorianCalendar(): XMLGregorianCalendar = {
    val gcal = new GregorianCalendar()
    datatypeFactory.newXMLGregorianCalendar(gcal)
  }

  // Helper to create a simple DcpAccessType
  private def createDcpAccessType(): DcpAccessType = {
    DcpAccessType(
      dcpaccesstypeoption = scalaxb.DataRecord[String](None, Some("all"), "")
    )
  }

  // Helper to create a basic DcpType (Data Collection Policy)
  private def createDcpType(): DcpType = {
    DcpType(
      access = createDcpAccessType(),
      statement = Seq.empty,
      expiry = None
    )
  }

  // Helper to create a SvcMenuType with standard EPP services
  private def createSvcMenuType(): SvcMenuType = {
    SvcMenuType(
      version = Seq(Number1u460), // Case object for version "1.0"
      lang = Seq("en", "uk"),
      objURI = Seq(
        new java.net.URI("urn:ietf:params:xml:ns:domain-1.0"),
        new java.net.URI("urn:ietf:params:xml:ns:contact-1.0"),
        new java.net.URI("urn:ietf:params:xml:ns:host-1.0")
      ),
      svcExtension = None
    )
  }

  /**
   * Returns a greeting response (typically after connection or login).
   */
  override def hello(): Future[GreetingType] = {
    Future.successful(
      GreetingType(
        svID = "EPP Mock Server",
        svDate = createXMLGregorianCalendar(),
        svcMenu = createSvcMenuType(),
        dcp = createDcpType()
      )
    )
  }

  /**
   * Handles login command and returns a greeting response.
   * According to EPP spec, login returns a greeting-like response.
   */
  override def login(loginType: LoginType): Future[GreetingType] = {
    Future.successful(
      GreetingType(
        svID = "EPP Mock Server - Authenticated",
        svDate = createXMLGregorianCalendar(),
        svcMenu = createSvcMenuType(),
        dcp = createDcpType()
      )
    )
  }

  /**
   * Handles logout command.
   */
  override def logout(): Future[ResponseType] = {
    Future.successful(
      ResponseType(
        result = Seq(
          ResultType(
            msg = MsgType(
              value = "Command completed successfully; ending session",
              attributes = Map(
                "@lang" -> scalaxb.DataRecord[String]("en")
              )
            ),
            resulttypeoption = Seq.empty,
            attributes = Map(
              "@code" -> scalaxb.DataRecord[ResultCodeType](Number1500)
            )
          )
        ),
        msgQ = None,
        resData = None,
        extension = None,
        trID = TrIDType(
          clTRID = Some("USER-" + System.currentTimeMillis()),
          svTRID = "MOCK-" + System.currentTimeMillis()
        )
      )
    )
  }

  /**
   * Checks domain availability.
   *
   * Mock behavior:
   * - Domains ending with ".available" are marked as available (avail=true)
   * - Domains ending with ".taken" are marked as unavailable with reason "Object exists"
   * - All other domains are marked as unavailable with reason "Incorrect domain name"
   */
  override def domainCheck(checkType: MNameTypeType): Future[ChkDataTypeType2] = {
    val domainNames = checkType.name

    val checkResults = domainNames.map { domainName =>
      val (isAvailable, reason) = if (domainName.endsWith(".available")) {
        (true, None)
      } else if (domainName.endsWith(".taken")) {
        (false, Some(ReasonType(value = "Object exists")))
      } else {
        (false, Some(ReasonType(value = "Incorrect domain name")))
      }

      CheckTypeType2(
        name = CheckNameTypeType(
          value = domainName,
          attributes = Map(
            "@avail" -> scalaxb.DataRecord[Boolean](isAvailable)
          )
        ),
        reason = reason
      )
    }

    Future.successful(
      ChkDataTypeType2(cd = checkResults)
    )
  }

  // Stub implementations for other domain operations
  override def domainCreate(createType: CreateTypeType2): Future[CreDataTypeType2] = {
    if (createType.name.endsWith(".taken")) {
      Future.failed(EppErrorException(
        msg = "Object exists",
        code = Number2302
      ))
    } else if (createType.name.endsWith(".available")) {
      Future.successful(
        CreDataTypeType2(
          name = createType.name,
          crDate = createXMLGregorianCalendar()
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Invalid domain name",
        code = Number2005
      ))
    }
  }

  override def domainInfo(infoType: InfoType): Future[InfDataTypeType2] = {
    if (infoType.name.value.endsWith(".taken")) {
      Future.successful(
        InfDataTypeType2(
          name = infoType.name.value,
          roid = "ROID-" + System.currentTimeMillis(),
          clID = "testclient",
          status = Seq(
            StatusTypeType2(
              value = "ok",
              attributes = Map(
                "@s" -> scalaxb.DataRecord[StatusValueTypeType2](OkValue2),
                "@lang" -> scalaxb.DataRecord[String]("en")
              )
            )
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def domainUpdate(updateType: UpdateTypeType2): Future[ResponseType] = {
    if (updateType.name.endsWith(".taken")) {
      Future.successful(
        ResponseType(
          result = Seq(
            ResultType(
              msg = MsgType(
                value = "Command completed successfully",
                attributes = Map(
                  "@lang" -> scalaxb.DataRecord[String]("en")
                )
              ),
              resulttypeoption = Seq.empty,
              attributes = Map(
                "@code" -> scalaxb.DataRecord[ResultCodeType](Number1000)
              )
            )
          ),
          trID = TrIDType(
            clTRID = Some("USER-" + System.currentTimeMillis()),
            svTRID = "MOCK-" + System.currentTimeMillis()
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def domainDelete(deleteType: SNameTypeType): Future[ResponseType] = {
    if (deleteType.name.endsWith(".taken")) {
      Future.successful(
        ResponseType(
          result = Seq(
            ResultType(
              msg = MsgType(
                value = "Command completed successfully",
                attributes = Map(
                  "@lang" -> scalaxb.DataRecord[String]("en")
                )
              ),
              resulttypeoption = Seq.empty,
              attributes = Map(
                "@code" -> scalaxb.DataRecord[ResultCodeType](Number1000)
              )
            )
          ),
          trID = TrIDType(
            clTRID = Some("USER-" + System.currentTimeMillis()),
            svTRID = "MOCK-" + System.currentTimeMillis()
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def domainTransfer(transferType: TransferType): Future[TrnDataTypeType] = {
    val op = transferType.op
    val transferTypeType = transferType.any.as[TransferTypeType]
    op match {
      case Request =>
        Future.successful(
          TrnDataTypeType(
            name = transferTypeType.name,
            trStatus = Pending,
            reID = "testclient",
            reDate = createXMLGregorianCalendar(),
            acID = "testclient",
            acDate = createXMLGregorianCalendar()
          )
        )
      case Query =>
        Future.successful(
          TrnDataTypeType(
            name = transferTypeType.name,
            trStatus = ServerApproved,
            reID = "testclient",
            reDate = createXMLGregorianCalendar(),
            acID = "testclient",
            acDate = createXMLGregorianCalendar()
          )
        )
      case Approve =>
        Future.successful(
          TrnDataTypeType(
            name = transferTypeType.name,
            trStatus = ServerApproved,
            reID = "testclient",
            reDate = createXMLGregorianCalendar(),
            acID = "testclient",
            acDate = createXMLGregorianCalendar()
          )
        )
      case Reject =>
        Future.successful(
          TrnDataTypeType(
            name = transferTypeType.name,
            trStatus = ServerCancelled,
            reID = "testclient",
            reDate = createXMLGregorianCalendar(),
            acID = "testclient",
            acDate = createXMLGregorianCalendar()
          )
        )
      case Cancel =>
        Future.successful(
          TrnDataTypeType(
            name = transferTypeType.name,
            trStatus = ServerCancelled,
            reID = "testclient",
            reDate = createXMLGregorianCalendar(),
            acID = "testclient",
            acDate = createXMLGregorianCalendar()
          )
        )
      case _ =>
        Future.failed(new UnsupportedOperationException("Unsupported transfer operation"))
    }
  }

  override def domainRenew(renewType: RenewType): Future[RenDataType] = {
    if (renewType.name.endsWith(".taken")) {
      Future.successful(
        RenDataType(
          name = renewType.name,
          exDate = Some(createXMLGregorianCalendar())
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def executeCommand(command: CommandType): Future[ResponseType] = {
    Future.failed(new UnsupportedOperationException("executeCommand not implemented in mock"))
  }

  // Stub implementations for contact operations
  override def contactCheck(checkType: MIDType): Future[ChkDataTypeType] = {
    val contactIds = checkType.id
    val checkResults = contactIds.map { contactId =>
      val isAvailable = if (contactId == "sh8013") {
        true
      } else {
        false
      }
      CheckTypeType(
        id = CheckIDType(
          value = contactId,
          attributes = Map(
            "@avail" -> scalaxb.DataRecord(isAvailable)
          )
        )
      )
    }
    Future.successful(
      ChkDataTypeType(cd = checkResults)
    )
  }

  override def contactCreate(createType: CreateTypeType): Future[CreDataTypeType] = {
    if (createType.id == "sh8013") {
      Future.successful(
        CreDataTypeType(
          id = createType.id,
          crDate = createXMLGregorianCalendar()
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object exists",
        code = Number2302
      ))
    }
  }

  override def contactInfo(infoType: AuthIDType): Future[InfDataTypeType] = {
    if (infoType.id == "sh8013") {
      Future.successful(
        InfDataTypeType(
          id = infoType.id,
          roid = "ROID-" + System.currentTimeMillis(),
          clID = "testclient",
          crID = "testclient",
          crDate = createXMLGregorianCalendar(),
          email = "sh8013@example.com",
          status = Seq(
            StatusTypeType(
              value = "ok",
              attributes = Map(
                "@s" -> scalaxb.DataRecord[StatusValueTypeType](OkValue),
                "@lang" -> scalaxb.DataRecord[String]("en")
              )
            )
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def contactUpdate(updateType: UpdateTypeType): Future[ResponseType] = {
    if (updateType.id == "sh8013") {
      Future.successful(
        ResponseType(
          result = Seq(
            ResultType(
              msg = MsgType(
                value = "Command completed successfully",
                attributes = Map(
                  "@lang" -> scalaxb.DataRecord[String]("en")
                )
              ),
              resulttypeoption = Seq.empty,
              attributes = Map(
                "@code" -> scalaxb.DataRecord[ResultCodeType](Number1000)
              )
            )
          ),
          trID = TrIDType(
            clTRID = Some("USER-" + System.currentTimeMillis()),
            svTRID = "MOCK-" + System.currentTimeMillis()
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def contactDelete(deleteType: SIDType): Future[ResponseType] = {
    if (deleteType.id == "sh8013") {
      Future.successful(
        ResponseType(
          result = Seq(
            ResultType(
              msg = MsgType(
                value = "Command completed successfully",
                attributes = Map(
                  "@lang" -> scalaxb.DataRecord[String]("en")
                )
              ),
              resulttypeoption = Seq.empty,
              attributes = Map(
                "@code" -> scalaxb.DataRecord[ResultCodeType](Number1000)
              )
            )
          ),
          trID = TrIDType(
            clTRID = Some("USER-" + System.currentTimeMillis()),
            svTRID = "MOCK-" + System.currentTimeMillis()
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def contactTransfer(transferType: AuthIDType): Future[TrnDataType] = {
    if (transferType.id == "sh8013") {
      Future.successful(
        TrnDataType(
          id = transferType.id,
          trStatus = ServerApproved,
          reID = "testclient",
          reDate = createXMLGregorianCalendar(),
          acID = "testclient",
          acDate = createXMLGregorianCalendar()
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  // Stub implementations for host operations
  override def hostCheck(checkType: MNameType): Future[ChkDataType] = {
    val hostNames = checkType.name
    val checkResults = hostNames.map { hostName =>
      val isAvailable = if (hostName.endsWith(".com")) {
        true
      } else {
        false
      }
      CheckType(
        name = CheckNameType(
          value = hostName,
          attributes = Map(
            "@avail" -> scalaxb.DataRecord(isAvailable)
          )
        ),
        reason = None
      )
    }
    Future.successful(
      ChkDataType(cd = checkResults)
    )
  }

  override def hostCreate(createType: CreateType): Future[CreDataType] = {
    if (createType.name.endsWith(".com")) {
      Future.successful(
        CreDataType(
          name = createType.name,
          crDate = createXMLGregorianCalendar()
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object exists",
        code = Number2302
      ))
    }
  }

  override def hostInfo(infoType: SNameType): Future[InfDataType] = {
    if (infoType.name.endsWith(".com")) {
      Future.successful(
        InfDataType(
          name = infoType.name,
          roid = "ROID-" + System.currentTimeMillis(),
          clID = "testclient",
          crID = "testclient",
          crDate = createXMLGregorianCalendar(),
          status = Seq(
            StatusType(
              value = "ok",
              attributes = Map(
                "@s" -> scalaxb.DataRecord[StatusValueType](Ok),
                "@lang" -> scalaxb.DataRecord[String]("en")
              )
            )
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def hostUpdate(updateType: UpdateType): Future[ResponseType] = {
    if (updateType.name.endsWith(".com")) {
      Future.successful(
        ResponseType(
          result = Seq(
            ResultType(
              msg = MsgType(
                value = "Command completed successfully",
                attributes = Map(
                  "@lang" -> scalaxb.DataRecord[String]("en")
                )
              ),
              resulttypeoption = Seq.empty,
              attributes = Map(
                "@code" -> scalaxb.DataRecord[ResultCodeType](Number1000)
              )
            )
          ),
          trID = TrIDType(
            clTRID = Some("USER-" + System.currentTimeMillis()),
            svTRID = "MOCK-" + System.currentTimeMillis()
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }

  override def hostDelete(deleteType: SNameType): Future[ResponseType] = {
    if (deleteType.name.endsWith(".com")) {
      Future.successful(
        ResponseType(
          result = Seq(
            ResultType(
              msg = MsgType(
                value = "Command completed successfully",
                attributes = Map(
                  "@lang" -> scalaxb.DataRecord[String]("en")
                )
              ),
              resulttypeoption = Seq.empty,
              attributes = Map(
                "@code" -> scalaxb.DataRecord[ResultCodeType](Number1000)
              )
            )
          ),
          trID = TrIDType(
            clTRID = Some("USER-" + System.currentTimeMillis()),
            svTRID = "MOCK-" + System.currentTimeMillis()
          )
        )
      )
    } else {
      Future.failed(EppErrorException(
        msg = "Object does not exist",
        code = Number2303
      ))
    }
  }
}
