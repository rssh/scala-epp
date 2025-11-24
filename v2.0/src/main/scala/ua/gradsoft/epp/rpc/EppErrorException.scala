package ua.gradsoft.epp.rpc

import ua.gradsoft.epp.xsdmodel.ResultCodeType

/**
 * Exception thrown when an EPP command fails.
 *
 * @param msg  the error message
 * @param code the EPP result code
 */
case class EppErrorException(msg: String, code: ResultCodeType) extends RuntimeException(s"$code: $msg")
