package ua.gradsoft.epp.util

import java.time.Instant
import javax.xml.datatype.XMLGregorianCalendar

object EppXmlUtil {

  def fromXMLGregorianCalendar(cal: XMLGregorianCalendar): Instant = {
    cal.toGregorianCalendar.toInstant
  }

}
