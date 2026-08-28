package ua.gradsoft.epp

import scala.concurrent.{ExecutionContext, Future}
import ua.gradsoft.epp.model._
import ua.gradsoft.epp.rpc.EppRpcService
import ua.gradsoft.epp.xsdmodel.{ContactType => XsdContactType, PostalInfoType => XsdPostalInfoType, _}

class EppConnectionImpl(
  rpcService: EppRpcService
)(implicit ec: ExecutionContext) extends EppConnection {

  private val eppLogger = org.slf4j.LoggerFactory.getLogger("ua.gradsoft.epp")

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

  /**
   * .ua attaches nameservers as host objects, so a name must already exist in the registry before
   * a domain can reference it with <domain:hostObj> - otherwise the whole operation is refused
   * with 2303 "object does not exist", naming neither the host nor the reason. Create the ones
   * that are missing, run `command`, and take every host this call created back out again if
   * anything after it fails: a host object left behind by a domain operation that never happened
   * is invisible to the portal and has to be cleaned up at the registry by hand.
   *
   * Hosts outside .ua carry no addresses: the registry resolves them itself and refuses with 2306
   * if they do not resolve. Hosts inside a .ua zone need glue, which a list of nameserver names
   * cannot carry - see hostCreateRefused for what the caller is told when one of those is missing.
   *
   * Sequential on purpose: the RPC layer serialises every exchange on the one connection anyway,
   * so issuing these concurrently would only queue threads behind that lock.
   */
  private def withEnsuredHosts[T](domainName: String, names: Seq[String])(command: => Future[T]): Future[T] = {
    val created = scala.collection.mutable.ArrayBuffer.empty[String]

    val ensured = names.distinct.foldLeft(Future.successful(())) { (previous, name) =>
      previous.flatMap { _ =>
        checkHost(name).flatMap { available =>
          // checkHost reports availability: available means nothing is registered under the name.
          if available then
            rpcService.hostCreate(CreateType(name = name, addr = Seq.empty))
              .map { _ => created += name; () }
              .recoverWith { case e => Future.failed(hostCreateRefused(domainName, name, e)) }
          else Future.successful(())
        }
      }
    }

    ensured.flatMap(_ => command).recoverWith { case e =>
      discardHosts(created.toSeq).flatMap(_ => Future.failed(e))
    }
  }

  /**
   * A refused hostCreate reads as an error about a name the caller never asked to create. Say
   * which domain it was for, and for a name inside .ua add what the registry will not: a host
   * under a .ua zone is created with glue addresses, and a nameserver list has none to give, so
   * that host has to exist before a domain can point at it.
   *
   * The EPP result code is carried through unchanged - callers route on it.
   */
  private def hostCreateRefused(domainName: String, host: String, cause: Throwable): Throwable = {
    val glue =
      if host.toLowerCase.endsWith(".ua") then
        s"; $host is inside .ua, where a host object needs glue addresses - create it at the registry with its IP addresses first"
      else ""
    val what = s"cannot create nameserver host $host for $domainName"
    cause match {
      case e: ua.gradsoft.epp.rpc.EppErrorException =>
        ua.gradsoft.epp.rpc.EppErrorException(s"$what: ${e.msg}$glue", e.code)
      case e =>
        new RuntimeException(s"$what: ${e.getMessage}$glue", e)
    }
  }

  /**
   * Best effort: the failure that brought us here is what the caller has to see, so a host that
   * will not go away is logged and left.
   */
  private def discardHosts(names: Seq[String]): Future[Unit] =
    names.foldLeft(Future.successful(())) { (previous, name) =>
      previous.flatMap { _ =>
        deleteHost(name).recover { case e =>
          eppLogger.warn(
            s"host $name was created for a domain operation that then failed, and could not be removed: ${e.getMessage}"
          )
        }
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

    withEnsuredHosts(domain.name, domain.nameservers) {
      rpcService.domainCreate(createType).map { result =>
        domain.copy(
          creationDate = Some(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar(result.crDate)),
          expirationDate = result.exDate.map(ua.gradsoft.epp.util.EppXmlUtil.fromXMLGregorianCalendar)
        )
      }
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
      // 2303 is EPP's "object does not exist" - the only error that legitimately means None.
      // Every other error (auth, authorization, syntax, server failure) must reach the caller:
      // reporting them as "not found" hides the reason and sends the caller down a wrong path.
      case e: ua.gradsoft.epp.rpc.EppErrorException if e.code == Number2303 => None
    }
  }

  override def updateDomain(domain: DomainInfo): Future[DomainInfo] = {
    // Fetch current EPP state to compute diff
    infoDomain(domain.name, domain.authInfo).flatMap {
      case None =>
        Future.failed(new IllegalStateException(s"Domain ${domain.name} not found in EPP registry"))
      case Some(current) =>
        val currentNs = current.nameservers.toSet
        val desiredNs = domain.nameservers.toSet
        val nsToAdd = desiredNs -- currentNs
        val nsToRemove = currentNs -- desiredNs

        val currentContacts = current.contacts.map(c => (c.contactType, c.id)).toSet
        val desiredContacts = domain.contacts.map(c => (c.contactType, c.id)).toSet
        val contactsToAdd = desiredContacts -- currentContacts
        val contactsToRemove = currentContacts -- desiredContacts

        def makeNsType(ns: Set[String]): Option[NsType] =
          if ns.isEmpty then None
          else Some(NsType(nstypeoption = ns.toSeq.map { n =>
            scalaxb.DataRecord[String](Some("http://hostmaster.ua/epp/domain-1.1"), Some("hostObj"), n)
          }))

        def makeContacts(contacts: Set[(Option[ContactType], String)]): Seq[XsdContactType] =
          contacts.toSeq.map { case (ct, id) =>
            val attr = ct match {
              case Some(ContactType.Admin)   => Admin
              case Some(ContactType.Billing) => Billing
              case _                         => Tech
            }
            XsdContactType(value = id, attributes = Map("@type" -> scalaxb.DataRecord[ContactAttrType](attr)))
          }

        def makeAddRem(ns: Set[String], contacts: Set[(Option[ContactType], String)]): Option[AddRemTypeType2] = {
          val nsOpt = makeNsType(ns)
          val contactSeq = makeContacts(contacts)
          if nsOpt.isEmpty && contactSeq.isEmpty then None
          else Some(AddRemTypeType2(ns = nsOpt, contact = contactSeq))
        }

        val registrantChanged = domain.registrant.isDefined && domain.registrant != current.registrant
        val chg: Option[ChgTypeType] =
          if registrantChanged then Some(ChgTypeType(registrant = domain.registrant))
          else None

        val add = makeAddRem(nsToAdd, contactsToAdd)
        val rem = makeAddRem(nsToRemove, contactsToRemove)

        if add.isEmpty && rem.isEmpty && chg.isEmpty then
          // Nothing to change — skip the EPP call
          Future.successful(domain)
        else
          withEnsuredHosts(domain.name, nsToAdd.toSeq) {
            val updateType = UpdateTypeType2(name = domain.name, add = add, rem = rem, chg = chg)
            rpcService.domainUpdate(updateType).map(_ => domain)
          }
    }
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

    def toXsdPostalInfo(pi: PostalInfo): XsdPostalInfoType =
      val typeAttr: PostalInfoEnumType = pi.infoType match
        case PostalInfoType.Local          => Loc
        case PostalInfoType.International  => IntType
      XsdPostalInfoType(
        name = pi.name,
        org  = pi.organization,
        addr = AddrTypeType(
          street = pi.address.street,
          city   = pi.address.city,
          sp     = pi.address.stateProvince,
          pc     = pi.address.postalCode,
          cc     = pi.address.countryCode
        ),
        attributes = Map("@type" -> scalaxb.DataRecord[PostalInfoEnumType](typeAttr))
      )

    val createType = CreateTypeType(
      id         = contact.id,
      postalInfo = contact.postalInfo.map(toXsdPostalInfo),
      voice      = contact.voice.map(p => E164Type(p.number, p.extension.map(x => Map("@x" -> scalaxb.DataRecord[String](x))).getOrElse(Map.empty))),
      fax        = contact.fax.map(p => E164Type(p.number, p.extension.map(x => Map("@x" -> scalaxb.DataRecord[String](x))).getOrElse(Map.empty))),
      email      = contact.email,
      authInfo   = authInfoType,
      disclose   = None
    )

    rpcService.contactCreate(createType).map { result =>
      contact.copy(
        id = result.id,
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
      // 2303 is EPP's "object does not exist" - the only error that legitimately means None.
      // Every other error (auth, authorization, syntax, server failure) must reach the caller:
      // reporting them as "not found" hides the reason and sends the caller down a wrong path.
      case e: ua.gradsoft.epp.rpc.EppErrorException if e.code == Number2303 => None
    }
  }

  override def updateContact(contact: ContactInfo): Future[ContactInfo] = {
    // Fetch current EPP state to know which postal info types exist in the registry.
    // The chg section can only modify existing postal info types — it cannot add new ones
    // (contact add/rem only supports status changes, not postal info).
    infoContact(contact.id, None).flatMap {
      case None =>
        Future.failed(new IllegalStateException(s"Contact ${contact.id} not found in EPP registry"))
      case Some(current) =>
        val existingTypes = current.postalInfo.map(_.infoType).toSet

        def toChgPostalInfo(pi: PostalInfo): ChgPostalInfoType = {
          val typeAttr: PostalInfoEnumType = pi.infoType match
            case PostalInfoType.Local         => Loc
            case PostalInfoType.International => IntType
          ChgPostalInfoType(
            name = Some(pi.name),
            org  = pi.organization,
            addr = Some(AddrTypeType(
              street = pi.address.street,
              city   = pi.address.city,
              sp     = pi.address.stateProvince,
              pc     = pi.address.postalCode,
              cc     = pi.address.countryCode
            )),
            attributes = Map("@type" -> scalaxb.DataRecord[PostalInfoEnumType](typeAttr))
          )
        }

        def toE164(p: PhoneNumber): E164Type =
          E164Type(p.number, p.extension.map(x => Map("@x" -> scalaxb.DataRecord[String](x))).getOrElse(Map.empty))

        // A contact must have at least one postal info type.
        if contact.postalInfo.isEmpty then
          Future.failed(new IllegalArgumentException("Contact must have at least one postal info (int or loc)"))
        else
          def spacePostalInfo(infoType: PostalInfoType): PostalInfo =
            PostalInfo(infoType = infoType, name = "  ", organization = None,
              address = ContactAddress(street = Seq("  "), city = "  ", stateProvince = None, postalCode = None, countryCode = "UA"))

          // For each type that existed in EPP but is absent from the desired state,
          // inject a space-filled entry — the registry interprets spaces as clearing that type.
          val clearLoc = existingTypes.contains(PostalInfoType.Local) &&
            !contact.postalInfo.exists(_.infoType == PostalInfoType.Local)
          val clearInt = existingTypes.contains(PostalInfoType.International) &&
            !contact.postalInfo.exists(_.infoType == PostalInfoType.International)

          val postalInfoToChg = contact.postalInfo ++
            (if clearLoc then Seq(spacePostalInfo(PostalInfoType.Local)) else Seq.empty) ++
            (if clearInt then Seq(spacePostalInfo(PostalInfoType.International)) else Seq.empty)

          val chg = ChgType(
            postalInfo = postalInfoToChg.map(toChgPostalInfo),
            voice      = contact.voice.map(toE164),
            fax        = contact.fax.map(toE164),
            email      = Some(contact.email)
          )

          val updateType = UpdateTypeType(
            id  = contact.id,
            add = None,
            rem = None,
            chg = Some(chg)
          )
          rpcService.contactUpdate(updateType).map(_ => contact)
    }
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
      // 2303 is EPP's "object does not exist" - the only error that legitimately means None.
      // Every other error (auth, authorization, syntax, server failure) must reach the caller:
      // reporting them as "not found" hides the reason and sends the caller down a wrong path.
      case e: ua.gradsoft.epp.rpc.EppErrorException if e.code == Number2303 => None
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
