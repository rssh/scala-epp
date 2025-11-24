package ua.gradsoft.epp.rpc

import scala.concurrent.Future
import ua.gradsoft.epp.xsdmodel._

trait EppRpcService {

  // Session management
  def hello(): Future[GreetingType] // Added hello command
  def login(loginType: LoginType): Future[GreetingType]
  def logout(): Future[ResponseType] // Logout typically doesn't take a specific type, just logs out

  // Domain operations
  def domainCheck(checkType: MNameTypeType): Future[ChkDataTypeType2]
  def domainCreate(createType: CreateTypeType2): Future[CreDataTypeType2]
  def domainInfo(infoType: InfoType): Future[InfDataTypeType2]
  def domainUpdate(updateType: UpdateTypeType2): Future[ResponseType] // Update usually returns a general response
  def domainDelete(deleteType: SNameTypeType): Future[ResponseType]
  def domainTransfer(transferType: TransferType): Future[TrnDataTypeType]
  def domainRenew(renewType: RenewType): Future[RenDataType]

  def executeCommand(command: CommandType): Future[ResponseType]

  // Contact operations
  def contactCheck(checkType: MIDType): Future[ChkDataTypeType]
  def contactCreate(createType: CreateTypeType): Future[CreDataTypeType]
  def contactInfo(infoType: AuthIDType): Future[InfDataTypeType]
  def contactUpdate(updateType: UpdateTypeType): Future[ResponseType]
  def contactDelete(deleteType: SIDType): Future[ResponseType]
  def contactTransfer(transferType: AuthIDType): Future[TrnDataType]

  // Host operations
  def hostCheck(checkType: MNameType): Future[ChkDataType]
  def hostCreate(createType: CreateType): Future[CreDataType]
  def hostInfo(infoType: SNameType): Future[InfDataType]
  def hostUpdate(updateType: UpdateType): Future[ResponseType]
  def hostDelete(deleteType: SNameType): Future[ResponseType]

}