package ua.gradsoft.epp.rpc

import scala.concurrent.{ExecutionContext, Future}
import ua.gradsoft.epp.xsdmodel._
import scalaxb.DataRecord

trait EppRpcServiceImpl extends EppRpcService {

  implicit def executionContext: ExecutionContext

  private val eppNs = Some("urn:ietf:params:xml:ns:epp-1.0")
  private val domainNs = Some("http://hostmaster.ua/epp/domain-1.1")
  private val contactNs = Some("http://hostmaster.ua/epp/contact-1.1")
  private val hostNs = Some("http://hostmaster.ua/epp/host-1.1")

  // Abstract method - must be implemented by concrete class
  def processEppMessage(input: EppType): Future[EppType]

  // Session management
  override def hello(): Future[GreetingType] = {
    val helloRecord = DataRecord(eppNs, Some("hello"), "")
    val eppRequest = EppType(helloRecord)
    processEppMessage(eppRequest).flatMap { response =>
      extractEppContent[GreetingType](response, "greeting")
    }
  }

  override def login(loginType: LoginType): Future[ResponseType] = {
    val loginRecord = DataRecord(eppNs, Some("login"), None, None, loginType)
    val command = CommandType(loginRecord, None, None)
    executeCommand(command)
  }

  override def logout(): Future[ResponseType] = {
    val logoutRecord = DataRecord(eppNs, Some("logout"), "")
    val command = CommandType(logoutRecord, None, None)
    executeCommand(command)
  }

  override def executeCommand(command: CommandType): Future[ResponseType] = {
    val commandRecord = DataRecord(eppNs, Some("command"), None, None, command)
    val eppRequest = EppType(commandRecord)
    processEppMessage(eppRequest).flatMap { response =>
      extractEppContent[ResponseType](response, "response")
    }
  }

  // Domain operations
  override def domainCheck(checkType: MNameTypeType): Future[ChkDataTypeType2] = {
    val checkRecord = DataRecord(domainNs, Some("check"), None, None, checkType)
    val command = CommandType(checkRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[ChkDataTypeType2](response, domainNs, "chkData")
    }
  }

  override def domainCreate(createType: CreateTypeType2): Future[CreDataTypeType2] = {
    val createRecord = DataRecord(domainNs, Some("create"), None, None, createType)
    val command = CommandType(createRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[CreDataTypeType2](response, domainNs, "creData")
    }
  }

  override def domainInfo(infoType: InfoType): Future[InfDataTypeType2] = {
    val infoRecord = DataRecord(domainNs, Some("info"), None, None, infoType)
    val command = CommandType(infoRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[InfDataTypeType2](response, domainNs, "infData")
    }
  }

  override def domainUpdate(updateType: UpdateTypeType2): Future[ResponseType] = {
    val updateRecord = DataRecord(domainNs, Some("update"), None, None, updateType)
    val command = CommandType(updateRecord, None, None)
    executeCommand(command)
  }

  override def domainDelete(deleteType: SNameTypeType): Future[ResponseType] = {
    val deleteRecord = DataRecord(domainNs, Some("delete"), None, None, deleteType)
    val command = CommandType(deleteRecord, None, None)
    executeCommand(command)
  }

  override def domainTransfer(transferType: TransferType): Future[TrnDataTypeType] = {
    val transferRecord = DataRecord(domainNs, Some("transfer"), None, None, transferType)
    val command = CommandType(transferRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[TrnDataTypeType](response, domainNs, "trnData")
    }
  }

  override def domainRenew(renewType: RenewType): Future[RenDataType] = {
    val renewRecord = DataRecord(domainNs, Some("renew"), None, None, renewType)
    val command = CommandType(renewRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[RenDataType](response, domainNs, "renData")
    }
  }

  // Contact operations
  override def contactCheck(checkType: MIDType): Future[ChkDataTypeType] = {
    val checkRecord = DataRecord(contactNs, Some("check"), None, None, checkType)
    val command = CommandType(checkRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[ChkDataTypeType](response, contactNs, "chkData")
    }
  }

  override def contactCreate(createType: CreateTypeType): Future[CreDataTypeType] = {
    val createRecord = DataRecord(contactNs, Some("create"), None, None, createType)
    val command = CommandType(createRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[CreDataTypeType](response, contactNs, "creData")
    }
  }

  override def contactInfo(infoType: AuthIDType): Future[InfDataTypeType] = {
    val infoRecord = DataRecord(contactNs, Some("info"), None, None, infoType)
    val command = CommandType(infoRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[InfDataTypeType](response, contactNs, "infData")
    }
  }

  override def contactUpdate(updateType: UpdateTypeType): Future[ResponseType] = {
    val updateRecord = DataRecord(contactNs, Some("update"), None, None, updateType)
    val command = CommandType(updateRecord, None, None)
    executeCommand(command)
  }

  override def contactDelete(deleteType: SIDType): Future[ResponseType] = {
    val deleteRecord = DataRecord(contactNs, Some("delete"), None, None, deleteType)
    val command = CommandType(deleteRecord, None, None)
    executeCommand(command)
  }

  override def contactTransfer(transferType: AuthIDType): Future[TrnDataType] = {
    val transferRecord = DataRecord(contactNs, Some("transfer"), None, None, transferType)
    val command = CommandType(transferRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[TrnDataType](response, contactNs, "trnData")
    }
  }

  // Host operations
  override def hostCheck(checkType: MNameType): Future[ChkDataType] = {
    val checkRecord = DataRecord(hostNs, Some("check"), None, None, checkType)
    val command = CommandType(checkRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[ChkDataType](response, hostNs, "chkData")
    }
  }

  override def hostCreate(createType: CreateType): Future[CreDataType] = {
    val createRecord = DataRecord(hostNs, Some("create"), None, None, createType)
    val command = CommandType(createRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[CreDataType](response, hostNs, "creData")
    }
  }

  override def hostInfo(infoType: SNameType): Future[InfDataType] = {
    val infoRecord = DataRecord(hostNs, Some("info"), None, None, infoType)
    val command = CommandType(infoRecord, None, None)
    executeCommand(command).flatMap { response =>
      extractResponseData[InfDataType](response, hostNs, "infData")
    }
  }

  override def hostUpdate(updateType: UpdateType): Future[ResponseType] = {
    val updateRecord = DataRecord(hostNs, Some("update"), None, None, updateType)
    val command = CommandType(updateRecord, None, None)
    executeCommand(command)
  }

  override def hostDelete(deleteType: SNameType): Future[ResponseType] = {
    val deleteRecord = DataRecord(hostNs, Some("delete"), None, None, deleteType)
    val command = CommandType(deleteRecord, None, None)
    executeCommand(command)
  }

  // Helper method to extract typed content from EppType response
  private def extractEppContent[T](eppResponse: EppType, expectedKey: String)(
    implicit format: scalaxb.XMLFormat[T]
  ): Future[T] = {
    val record = eppResponse.epptypeoption
    if (record.key.contains(expectedKey)) {
      try {
        Future.successful(record.as[T])
      } catch {
        case e: Exception =>
          Future.failed(new IllegalArgumentException(s"Failed to extract $expectedKey from EPP response: ${e.getMessage}", e))
      }
    } else {
      Future.failed(new IllegalArgumentException(s"Expected EPP element '$expectedKey' but got '${record.key.getOrElse("unknown")}'"))
    }
  }

  // Helper method to extract typed response data
  private def extractResponseData[T](response: ResponseType, namespace: Option[String], elementName: String)(
    implicit format: scalaxb.XMLFormat[T]
  ): Future[T] = {
    response.resData match {
      case Some(ExtAnyType(records)) =>
        records.find { record =>
          record.key.contains(elementName) && record.namespace == namespace
        } match {
          case Some(record) =>
            try {
              Future.successful(record.as[T])
            } catch {
              case e: Exception =>
                Future.failed(new IllegalArgumentException(s"Failed to extract $elementName from response: ${e.getMessage}", e))
            }
          case None =>
            Future.failed(new IllegalArgumentException(s"Expected response data element '$elementName' not found in namespace $namespace"))
        }
      case None =>
        Future.failed(new IllegalArgumentException("No response data in EPP response"))
    }
  }
}
