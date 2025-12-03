package ua.gradsoft.epp

import ua.gradsoft.epp.model.{DomainInfo, ContactInfo, ContactTransferResult, HostInfo, EppPeriod}
import scala.concurrent.Future

// Placeholder for credentials
case class EppCredentials(id: String, pw: String)

trait EppConnection {

  def logout(): Future[Unit]

  // Domain operations
  def checkDomain(domainName: String): Future[Boolean] // Returns true if available, false if not
  def createDomain(domain: DomainInfo, period: Option[EppPeriod] = None, authInfo: Option[String] = None): Future[DomainInfo]
  def infoDomain(domainName: String, authInfo: Option[String] = None): Future[Option[DomainInfo]]
  def updateDomain(domain: DomainInfo): Future[DomainInfo]
  def deleteDomain(domainName: String, authInfo: Option[String] = None): Future[Unit]
  def transferDomain(domainName: String, authInfo: Option[String] = None): Future[DomainInfo] // Simplified for now
  def renewDomain(domainName: String, period: EppPeriod): Future[DomainInfo]

  // Contact operations
  def checkContact(contactId: String): Future[Boolean]
  def createContact(contact: ContactInfo, authInfo: Option[String] = None): Future[ContactInfo]
  def infoContact(contactId: String, authInfo: Option[String] = None): Future[Option[ContactInfo]]
  def updateContact(contact: ContactInfo): Future[ContactInfo]
  def deleteContact(contactId: String, authInfo: Option[String] = None): Future[Unit]
  def transferContact(contactId: String, authInfo: Option[String] = None): Future[ContactTransferResult]

  // Host operations
  def checkHost(hostName: String): Future[Boolean]
  def createHost(host: HostInfo): Future[HostInfo]
  def infoHost(hostName: String): Future[Option[HostInfo]]
  def updateHost(host: HostInfo): Future[HostInfo]
  def deleteHost(hostName: String): Future[Unit]

}

trait EppService {
  def login(credentials: EppCredentials): Future[EppConnection]
}
