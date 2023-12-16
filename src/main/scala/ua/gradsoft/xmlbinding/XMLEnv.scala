package ua.gradsoft.xmlbinding

import java.util.regex.Pattern
import scala.xml.NamespaceBinding

enum NamespaceBindingMode {
  case ToElement
  case ToScope
}

case class XMLEnv(
  namespaces: Map[String,String] = Map.empty,                
  namespaceBinding: NamespaceBinding = XMLEnv.emptyNamespaceBindings,
  namespaceBindingMode: NamespaceBindingMode = NamespaceBindingMode.ToElement,
  defaultNamespace: Option[String] = None,
  checkCorrectPrefixes: Boolean = true,
  isRootElement: Boolean = false,
)  {


  def withNamespace(uri: String, optPrefix: Option[String] = None): (String, XMLEnv) = {
    namespaces.get(uri) match {
      case Some(p) => (p, this)
      case None => {
        val prefix = newPrefix(optPrefix, uri)
        val next = copy(namespaces = namespaces + (uri -> prefix),
                        namespaceBinding = new NamespaceBinding(prefix, uri, namespaceBinding),
                        namespaceBindingMode = NamespaceBindingMode.ToElement)
        (prefix, next)
      }
    }
  }
  
  def bindNamespace(uri: String, prefix: Option[String]): WithXMLEnv[String] = {
    val (newPrefix, next) = withNamespace(uri, prefix)
    WithXMLEnv(newPrefix, next)
  }

  def withDefaultNamespace(uri: String): XMLEnv = {
    copy(defaultNamespace = Some(uri),
         namespaceBinding = new NamespaceBinding(null, uri, namespaceBinding))
  }

  
  def newPrefix(maybeString: Option[String], uri: String): String = {
    maybeString match {
      case Some(s) =>
        Option(namespaceBinding.getPrefix(s)) match {
          case Some(storedUri) =>
            if (storedUri == uri) then s else findNextPrefix(s)
          case None => s
        }
      case None =>
        findNextPrefix("ns")
    }
  }

  private def findNextPrefix(s:String): String = {
    quickFindPrefix(s).getOrElse(slowFindPrefix(s))
  }

  private def quickFindPrefix(tmpl:String): Option[String] = {
    val candidate = s"${tmpl}${namespaces.size}"
    if (namespaces.contains(candidate)) {
      None
    } else {
      Some(candidate)
    }
  }

  private def slowFindPrefix(tmpl:String): String = {
    val pattern = (Pattern.quote(tmpl) + "([0-9]*)").r
    val maxIndex = namespaces.keys.foldLeft(0) { (a,b) =>
      b match {
        case pattern(index) => Math.max(a,index.toInt)
        case _ => a
      }
    }
    s"${tmpl}${maxIndex+1}"
  }


}

object XMLEnv {
  def default: XMLEnv = XMLEnv()


  
  def emptyNamespaceBindings = new NamespaceBinding(null, null, null)
  
}

