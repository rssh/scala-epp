package ua.gradsoft.epp.model

import ua.gradsoft.epp.xsdmodel.AddrTypeType

/**
 * Contact address information
 * @param street Street address lines (up to 3)
 * @param city City name
 * @param stateProvince State or province
 * @param postalCode Postal code
 * @param countryCode Two-letter country code (ISO 3166-1 alpha-2)
 */
case class ContactAddress(
  street: Seq[String],
  city: String,
  stateProvince: Option[String],
  postalCode: Option[String],
  countryCode: String
)

object ContactAddress:
  def fromAddrType(addr: AddrTypeType): ContactAddress =
    ContactAddress(
      street = addr.street,
      city = addr.city,
      stateProvince = addr.sp,
      postalCode = addr.pc,
      countryCode = addr.cc
    )
