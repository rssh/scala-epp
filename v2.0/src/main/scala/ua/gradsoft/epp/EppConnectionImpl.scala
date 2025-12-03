package ua.gradsoft.epp

import scala.concurrent.{ExecutionContext, Future}
import ua.gradsoft.epp.model._
import ua.gradsoft.epp.rpc.EppRpcService
import ua.gradsoft.epp.xsdmodel.{ContactType => XsdContactType, _}

class EppConnectionImpl(
  rpcService: EppRpcService
)(implicit ec: ExecutionContext) extends EppConnection {

  override def logout(): Future[Unit] = {
    rpcService.logout().map(_ => ())
  }

  // Domain operations

  override def checkDomain(domainName: String): Future[Boolean] = {
    val checkType = MNameTypeType(name = Seq(domainName))
    rpcService.domainCheck(checkType).map { result =>
      result.cd.headOption.exists(_.name.avail)
    }
  }

  override def createDomain(domain: DomainInfo, period: Option[EppPeriod], authInfo: Option[String]): Future[DomainInfo] = {
    val periodType = period.map { p =>
      PeriodType(
        value = p.value,
        attributes = Map("@unit" -> scalaxb.DataRecord[PUnitType](Y))
      )
    }

    val nsType = if (domain.nameservers.nonEmpty) {
      Some(NsType(
        nstypeoption = domain.nameservers.map { ns =>
          scalaxb.DataRecord[String](Some("http://hostmaster.ua/epp/domain-1.1"), Some("hostObj"), ns)
        }
      ))
    } else None

    val contacts = domain.contacts.map { c =>
      val contactAttr = c.contactType match {
        case Some(ContactType.Admin) => Admin
        case Some(ContactType.Billing) => Billing
        case Some(ContactType.Tech) => Tech
        case None => Tech
      }
      XsdContactType(
        value = c.id,
        attributes = Map("@type" -> scalaxb.DataRecord[ContactAttrType](contactAttr))
      )
    }

    val authInfoType = authInfo.orElse(domain.authInfo).map { pw =>
      AuthInfoTypeType(
        authinfotypetypeoption = scalaxb.DataRecord[PwAuthInfoType](
          Some("http://hostmaster.ua/epp/domain-1.1"),
          Some("pw"),
          PwAuthInfoType(value = pw)
        )
      )
    }

    val createType = CreateTypeType2(
      name = domain.name,
      period = periodType,
      ns = nsType,
      registrant = domain.registrant.getOrElse(""),
      contact = contacts,
      authInfo = authInfoType
    )

    rpcService.domainCreate(createType).map { result =>
      domain.copy(
        creationDate = Some(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar(result.crDate)),
        expirationDate = result.exDate.map(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar)
      )
    }
  }

  override def infoDomain(domainName: String, authInfo: Option[String]): Future[Option[DomainInfo]] = {
    val authInfoType = authInfo.map { pw =>
      AuthInfoTypeType(
        authinfotypetypeoption = scalaxb.DataRecord[PwAuthInfoType](
          Some("http://hostmaster.ua/epp/domain-1.1"),
          Some("pw"),
          PwAuthInfoType(value = pw)
        )
      )
    }

    val infoType = InfoType(
      name = InfoNameType(
        value = domainName,
        attributes = Map("@hosts" -> scalaxb.DataRecord[HostsType](AllType))
      ),
      authInfo = authInfoType
    )

    rpcService.domainInfo(infoType).map { result =>
      Some(DomainInfo.fromInfDataType(result))
    }.recover {
      case _: ua.gradsoft.epp.rpc.EppErrorException => None
    }
  }

  override def updateDomain(domain: DomainInfo): Future[DomainInfo] = {
    val updateType = UpdateTypeType2(
      name = domain.name,
      add = None,
      rem = None,
      chg = None
    )
    rpcService.domainUpdate(updateType).map(_ => domain)
  }

  override def deleteDomain(domainName: String, authInfo: Option[String]): Future[Unit] = {
    val deleteType = SNameTypeType(name = domainName)
    rpcService.domainDelete(deleteType).map(_ => ())
  }

  override def transferDomain(domainName: String, authInfo: Option[String]): Future[DomainInfo] = {
    val authInfoType = authInfo.map { pw =>
      AuthInfoTypeType(
        authinfotypetypeoption = scalaxb.DataRecord[PwAuthInfoType](
          Some("http://hostmaster.ua/epp/domain-1.1"),
          Some("pw"),
          PwAuthInfoType(value = pw)
        )
      )
    }

    val transferTypeType = TransferTypeType(
      name = domainName,
      period = None,
      authInfo = authInfoType
    )

    val transferType = TransferType(
      any = scalaxb.DataRecord[TransferTypeType](
        Some("http://hostmaster.ua/epp/domain-1.1"),
        Some("transfer"),
        transferTypeType
      ),
      attributes = Map("@op" -> scalaxb.DataRecord[TransferOpType](Request))
    )

    rpcService.domainTransfer(transferType).map { result =>
      DomainInfo(
        name = result.name,
        roid = "",
        status = Seq.empty,
        registrant = None,
        contacts = Seq.empty,
        nameservers = Seq.empty,
        hosts = Seq.empty,
        sponsoringClientID = result.acID,
        creatingClientID = None,
        creationDate = None,
        lastUpdateClientID = None,
        lastUpdateDate = None,
        expirationDate = result.exDate.map(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar),
        lastTransferDate = Some(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar(result.acDate)),
        license = None,
        authInfo = None
      )
    }
  }

  override def renewDomain(domainName: String, period: EppPeriod): Future[DomainInfo] = {
    val periodType = PeriodType(
      value = period.value,
      attributes = Map("@unit" -> scalaxb.DataRecord[PUnitType](Y))
    )

    val renewType = RenewType(
      name = domainName,
      curExpDate = javax.xml.datatype.DatatypeFactory.newInstance().newXMLGregorianCalendar(),
      period = Some(periodType)
    )

    rpcService.domainRenew(renewType).map { result =>
      DomainInfo(
        name = result.name,
        roid = "",
        status = Seq.empty,
        registrant = None,
        contacts = Seq.empty,
        nameservers = Seq.empty,
        hosts = Seq.empty,
        sponsoringClientID = "",
        creatingClientID = None,
        creationDate = None,
        lastUpdateClientID = None,
        lastUpdateDate = None,
        expirationDate = result.exDate.map(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar),
        lastTransferDate = None,
        license = None,
        authInfo = None
      )
    }
  }

  // Contact operations

  override def checkContact(contactId: String): Future[Boolean] = {
    val checkType = MIDType(id = Seq(contactId))
    rpcService.contactCheck(checkType).map { result =>
      result.cd.headOption.exists(_.id.avail)
    }
  }

  override def createContact(contact: ContactInfo, authInfo: Option[String]): Future[ContactInfo] = {
    val authInfoType = AuthInfoType(
      authinfotypeoption = scalaxb.DataRecord[PwAuthInfoTypeType](
        Some("http://hostmaster.ua/epp/contact-1.1"),
        Some("pw"),
        PwAuthInfoTypeType(value = authInfo.getOrElse(""))
      )
    )

    val createType = CreateTypeType(
      id = contact.id,
      postalInfo = Seq.empty,
      voice = None,
      fax = None,
      email = contact.email,
      authInfo = authInfoType,
      disclose = None
    )

    rpcService.contactCreate(createType).map { result =>
      contact.copy(
        creationDate = ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar(result.crDate)
      )
    }
  }

  override def infoContact(contactId: String, authInfo: Option[String]): Future[Option[ContactInfo]] = {
    val authInfoType = authInfo.map { pw =>
      AuthInfoType(
        authinfotypeoption = scalaxb.DataRecord[PwAuthInfoTypeType](
          Some("http://hostmaster.ua/epp/contact-1.1"),
          Some("pw"),
          PwAuthInfoTypeType(value = pw)
        )
      )
    }

    val infoType = AuthIDType(
      id = contactId,
      authInfo = authInfoType
    )

    rpcService.contactInfo(infoType).map { result =>
      Some(ContactInfo.fromInfDataType(result))
    }.recover {
      case _: ua.gradsoft.epp.rpc.EppErrorException => None
    }
  }

  override def updateContact(contact: ContactInfo): Future[ContactInfo] = {
    val updateType = UpdateTypeType(
      id = contact.id,
      add = None,
      rem = None,
      chg = None
    )
    rpcService.contactUpdate(updateType).map(_ => contact)
  }

  override def deleteContact(contactId: String, authInfo: Option[String]): Future[Unit] = {
    val deleteType = SIDType(id = contactId)
    rpcService.contactDelete(deleteType).map(_ => ())
  }

  override def transferContact(contactId: String, authInfo: Option[String]): Future[ContactTransferResult] = {
    val authInfoType = authInfo.map { pw =>
      AuthInfoType(
        authinfotypeoption = scalaxb.DataRecord[PwAuthInfoTypeType](
          Some("http://hostmaster.ua/epp/contact-1.1"),
          Some("pw"),
          PwAuthInfoTypeType(value = pw)
        )
      )
    }

    val transferType = AuthIDType(
      id = contactId,
      authInfo = authInfoType
    )

    rpcService.contactTransfer(transferType).map(ContactTransferResult.fromTrnDataType)
  }

  // Host operations

  override def checkHost(hostName: String): Future[Boolean] = {
    val checkType = MNameType(name = Seq(hostName))
    rpcService.hostCheck(checkType).map { result =>
      result.cd.headOption.exists(_.name.avail)
    }
  }

  override def createHost(host: HostInfo): Future[HostInfo] = {
    val addresses = host.addresses.map { case (addr, ipType) =>
      AddrType(
        value = addr,
        attributes = Map("@ip" -> scalaxb.DataRecord[IpType](ipType))
      )
    }.toSeq

    val createType = CreateType(
      name = host.name,
      addr = addresses
    )

    rpcService.hostCreate(createType).map { result =>
      host.copy(
        creationDate = ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar(result.crDate)
      )
    }
  }

  override def infoHost(hostName: String): Future[Option[HostInfo]] = {
    val infoType = SNameType(name = hostName)

    rpcService.hostInfo(infoType).map { result =>
      Some(HostInfo.fromInfDataType(result))
    }.recover {
      case _: ua.gradsoft.epp.rpc.EppErrorException => None
    }
  }

  override def updateHost(host: HostInfo): Future[HostInfo] = {
    val updateType = UpdateType(
      name = host.name,
      add = None,
      rem = None
    )
    rpcService.hostUpdate(updateType).map(_ => host)
  }

  override def deleteHost(hostName: String): Future[Unit] = {
    val deleteType = SNameType(name = hostName)
    rpcService.hostDelete(deleteType).map(_ => ())
  }
}
