package ua.gradsoft.xmlbinding


import ua.gradsoft.xmlbinding.annotation.XMLNamespace

import scala.quoted.*
import scala.xml.Node
import scala.xml.NodeSeq

case class XMLCodecMacroParams(
  namespace: Option[String] = None,
  prefix: Option[String] = None
)

object XMLCodecMacro {

  def generateCodec[T:Type](using Quotes): Expr[XMLCodec[T]] = {
    generateCodecWithParams[T](XMLCodecMacroParams())
  }


  def generateCodecWithParams[T:Type](params: XMLCodecMacroParams)(using Quotes): Expr[XMLCodec[T]] = {
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    tpe match
      case AnnotatedType(underlying, annot) =>
        if (annot.tpe =:= TypeRepr.of[XMLNamespace]) then
          annot match
            case Apply(_, List(Literal(StringConstant(namespace)),Literal(StringConstant(prefix)))) =>
              underlying.asType match
                case '[t1] =>
                  val nParams = params.copy(namespace=Some(namespace),prefix=Some(prefix))
                  generateCodecWithParams[t1](nParams).asExprOf[XMLCodec[T]]
                case _ =>
                  report.throwError(s"internal error: underlying type for ${Type.show[T]} is not found",annot.pos)
            case _ =>
              report.throwError(s"bad shape for XMLNamespace annotation on ${Type.show[T]} - expected string literals",annot.pos)
        else
          underlying.asType match
            case '[t1] =>
              generateCodecWithParams[t1](params).asExprOf[XMLCodec[T]]
            case _ =>
              report.throwError(s"internal error: underlying type for ${Type.show[T]} is not found",annot.pos)
      case _ =>
         '{
            new XMLCodec[T] {
              def encode(value: T, env: XMLEnv): WithXMLEnv[NodeSeq] = ???
              def decode(nodeSeq:NodeSeq, env: XMLEnv): T = ???
            }
         }
  }


}
