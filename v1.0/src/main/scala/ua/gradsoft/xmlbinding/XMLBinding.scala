package ua.gradsoft.xmlbinding

object XMLBinding {

  def toXML[T](t:T)(using XMLCodec[T]): scala.xml.Node = {
    val xmlInEnv = summon[XMLCodec[T]].encode(t,XMLEnv.default)
    xmlInEnv.value.length match {
      case 0 => scala.xml.Text("")
      case 1 => xmlInEnv.value(0)
      case _ => scala.xml.Elem(null,"root",null,xmlInEnv.env.namespaceBinding, true, xmlInEnv.value:_*)
    }
  }

  def fromXML[T](xml:scala.xml.Node)(using XMLCodec[T]): T = {
    summon[XMLCodec[T]].decode(xml, XMLEnv.default)
  }

}
