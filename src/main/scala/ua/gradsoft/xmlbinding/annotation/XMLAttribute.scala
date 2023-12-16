package ua.gradsoft.xmlbinding.annotation

import scala.annotation.StaticAnnotation

/**
 * Specified, that this field in case class should be mapped to XML attribute.
 * name - name of attribute. If not specified, then name of field is used.
 **/
class XMLAttribute(name: String = "") extends StaticAnnotation {

}
