package ua.gradsoft.xmlbinding


import ua.gradsoft.xmlbinding
import ua.gradsoft.xmlbinding.NamespaceBindingMode.{ToElement, ToScope}
import ua.gradsoft.xmlbinding.annotation.XMLAttribute

import scala.xml.{Text, *}
import scala.quoted.*
import scala.util.control.NonFatal

trait XMLCodec[T] {
  
  def encode(value: T, env: XMLEnv): WithXMLEnv[NodeSeq]

  def decode(nodeSeq: NodeSeq, env:XMLEnv): T

}

object XMLCodec {

  inline def derived[T]: XMLCodec[T] = ${
    XMLCodecMacro.generateCodec[T]
  }

  object UnitXMLCodec extends XMLCodec[Unit] {

    def encode(value: Unit, env: XMLEnv): WithXMLEnv[NodeSeq] = {
      WithXMLEnv(NodeSeq.Empty, env)
    }

    def decode(nodeSeq: NodeSeq, env: XMLEnv): Unit = {
      ()
    }

  }

  class TextElementContentCodec[T](stringCodec: StringCodec[T]) extends XMLCodec[T] {

    def encode(value: T, env: XMLEnv): WithXMLEnv[NodeSeq] = {
      val text = Text(stringCodec.encode(value))
      WithXMLEnv(text, env)
    }

    def decode(nodeSeq: NodeSeq, env: XMLEnv): T = {
      nodeSeq match {
        case scala.xml.Text(text) => stringCodec.decode(text)
        case _ =>
          throw XMLCodecException("node shoulld be a text node", nodeSeq)
      }
    }

  }
  

}


trait XMLNodeCodec[T] extends XMLCodec[T] {

  def encodeNode(value: T, env: XMLEnv): WithXMLEnv[Node]
  
  def decodeNode(node: Node, env: XMLEnv): T
  
  override def encode(value: T, env: XMLEnv): WithXMLEnv[NodeSeq] = {
    val nodeInEnv = encodeNode(value, env)
    nodeInEnv.map(node => NodeSeq.fromSeq(node))
  }
  
  override def decode(nodeSeq: NodeSeq, env: XMLEnv): T = {
    nodeSeq match {
      case Seq(node: Node) => decodeNode(node, env)
      case _ =>
        throw XMLCodecException("node seq should have one element", nodeSeq)
    }
  }

}

object XMLNodeCodec {
  
  
  
}

trait XMLElementCodec[T] extends XMLNodeCodec[T] {

  def encodeElement(value: T, env: XMLEnv): WithXMLEnv[Elem]
  
  def decodeElement(node: Elem, env: XMLEnv): T
  
  override def encodeNode(value: T, env: XMLEnv): WithXMLEnv[Node] = {
    encodeElement(value, env)
  }
  
  override def decodeNode(nodeSeq: Node, env: XMLEnv): T = {
    nodeSeq match {
      case elem: Elem => decodeElement(elem, env)
      case _ =>
        throw XMLCodecException("node should be an element", nodeSeq)
    }
  }
  
}


class XMLAttributesCodecException(message: String, metaData: MetaData, cause: Throwable = null) extends RuntimeException(message, cause)

trait XMLAttributesCodec[T]  {

  def encodeAttributes(value: T, metaData: MetaData, env: XMLEnv): WithXMLEnv[MetaData]
  
  def decodeAttributes(metaData: MetaData, XMLEnv: XMLEnv): T
  
}



class XMLUnprefixedAttributeCodec[T:StringCodec](attrName: String) extends XMLAttributesCodec[T] {

  def encodeAttributes(value: T, metaData: MetaData, env:XMLEnv): WithXMLEnv[MetaData] = {
      val attr = new UnprefixedAttribute(attrName, summon[StringCodec[T]].encode(value), metaData)
      WithXMLEnv(attr, env)
  }

  def decodeAttributes(metaData: MetaData, XMLEnv: XMLEnv): T = {
    Option(metaData(attrName)) match {
      case Some(nodes) => nodes match {
        case Seq(node: Text) => 
          try
            summon[StringCodec[T]].decode(node.text)
          catch
            case NonFatal(ex) =>
              throw XMLAttributesCodecException(s"attribute $attrName has invalid value", metaData, ex)  
        case _ =>
          throw XMLAttributesCodecException(s"attribute $attrName should be a text node", metaData)
      }
      case None =>
        throw XMLAttributesCodecException(s"attribute $attrName not found", metaData)
    }
  }
  
}


class XMLOptionalUnprefixedAttributeCodec[T:StringCodec](attrName: String) extends XMLAttributesCodec[Option[T]] {

  def encodeAttributes(value: Option[T], metaData: MetaData, env: XMLEnv): WithXMLEnv[MetaData] = {
    val attr = value match {
      case Some(v) =>
        new UnprefixedAttribute(attrName, summon[StringCodec[T]].encode(v), metaData)
      case None =>
        metaData
    }
    WithXMLEnv(attr, env)
  }

  def decodeAttributes(metaData: MetaData, env: XMLEnv): Option[T] = {
    Option(metaData(attrName)) match {
      case Some(nodes) => nodes match {
        case Nil => None
        case Seq(node: Text) =>
          try
            Some(summon[StringCodec[T]].decode(node.text))
          catch
            case NonFatal(ex) =>
              throw XMLAttributesCodecException(s"attribute $attrName has invalid value", metaData, ex)
        case _ =>
          throw XMLAttributesCodecException(s"attribute $attrName should be a text node", metaData)
      }
      case None => None
    }
  }

}  

class XMLPrefixedAttributeCodec[T:StringCodec](uri: String, optPrefix:Option[String], attrName: String) extends XMLAttributesCodec[T] {

  def encodeAttributes(value: T, metaData: MetaData, env: XMLEnv): WithXMLEnv[MetaData] = {
    for {
      prefix <- env.bindNamespace(uri, optPrefix)
    } yield {
      PrefixedAttribute(prefix, attrName, summon[StringCodec[T]].encode(value), metaData)
    }
  }

  def decodeAttributes(metaData: MetaData, env: XMLEnv): T = {
    Option(metaData(uri,  env.namespaceBinding, attrName)) match {
      case Some(nodeSeq) =>
        nodeSeq match {
          case Seq(node: Text) =>
            try
              summon[StringCodec[T]].decode(node.text)
            catch
              case NonFatal(ex) =>
                throw XMLAttributesCodecException(s"attribute $attrName has invalid value", metaData, ex)
          case _ =>
            throw XMLAttributesCodecException(s"attribute $attrName should be a text node", metaData)
        }
      case None =>
        throw XMLAttributesCodecException(s"attribute $attrName is not found", metaData)
    }
    
  }

}
  
class SummaryAttrbuteCodec[A, B<:Tuple](aCodec: XMLAttributesCodec[A], bCodec: XMLAttributesCodec[B]) extends XMLAttributesCodec[A*:B] {

  override def encodeAttributes(value: A *: B, metaData: MetaData, env: XMLEnv): WithXMLEnv[MetaData] = {
    val aE = aCodec.encodeAttributes(value.head, metaData, env)
    val bE = bCodec.encodeAttributes(value.tail, metaData, aE.env)
    WithXMLEnv(bE.value, bE.env)
  }

  override def decodeAttributes(metaData: MetaData, env: XMLEnv): A*:B = {
    val a = aCodec.decodeAttributes(metaData, env)
    val b = bCodec.decodeAttributes(metaData, env)
    a *: b
  }

}

