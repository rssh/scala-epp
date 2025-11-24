package ua.gradsoft.xmlbinding

import scala.xml.NodeSeq

case class WithXMLEnv[+T](value: T, env: XMLEnv) {

  def map[S](f: T=>S): WithXMLEnv[S] = WithXMLEnv(f(value),env)

  def flatMap[S](f: T=>WithXMLEnv[S]): WithXMLEnv[S] = {
    val r = f(value)
    WithXMLEnv(r.value,r.env)
  }

  def updateEnv(f: XMLEnv=>XMLEnv): WithXMLEnv[T] = {
    WithXMLEnv(value,f(env))
  }
  
  
  
  
}

