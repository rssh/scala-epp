package ua.gradsoft.xmlbinding.annotation

/**
 * XML namespace annotation.
 *
 * @param namespace - namespace URI
 * @param defaultPrefix - preffered prefix. If not set - will be generated automatically.
 */
class XMLNamespace(namespace:String, defaultPrefix:String="") extends scala.annotation.StaticAnnotation


