package ua.gradsoft.xmlbinding

import scala.xml.*

/**
 * Exception, which is thrown, when we try to decode node, which is not valid for this codec.
 **/
class XMLCodecException(msg:String, nodeSeq: NodeSeq) extends RuntimeException(msg)

object XMLCodecException {

  class NodeTypeMismatch(node: Node) extends XMLCodecException("node type mismatch",node)
  class ShouldBeTextNode(node:Node) extends XMLCodecException("node should be text node",node)

}
