package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.{PostalInfoType, Loc, IntType}

/**
 * Postal info type - either local (native charset) or international (ASCII)
 */
enum PostalInfoType:
  case Local       // "loc" - local/native charset
  case International  // "int" - internationalized (ASCII)

/**
 * Contact postal information
 * @param infoType Type of postal info (local or international)
 * @param name Contact name
 * @param organization Organization name (optional)
 * @param address Contact address
 */
case class PostalInfo(
  infoType: PostalInfoType,
  name: String,
  organization: Option[String],
  address: ContactAddress
)

object PostalInfo:
  def fromPostalInfoType(info: ua.gradsoft.epp.xsdmodel.PostalInfoType): PostalInfo =
    val infoType = info.typeValue match
      case Loc => PostalInfoType.Local
      case IntType => PostalInfoType.International

    PostalInfo(
      infoType = infoType,
      name = info.name,
      organization = info.org,
      address = ContactAddress.fromAddrType(info.addr)
    )
