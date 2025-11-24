package ua.gradsoft.xmlbinding

import scala.xml.Text

class StringCodecException(msg:String, text: Text) extends XMLCodecException(msg,text)

trait StringCodec[T] {

  def encode(x:T):String;
  def decode(s:String):T;

}

object StringCodec {

   implicit object StringStringCodec extends StringCodec[String] {
     def encode(x:String):String = x;
     def decode(s:String):String = s;
   }
     
    implicit object IntStringCodec extends StringCodec[Int] {
      def encode(x:Int):String = x.toString;
      def decode(s:String):Int = try {
                                    s.toInt
                                  }catch{
                                    case ex:NumberFormatException => throw new StringCodecException("bad integer format",Text(s))
                                  }
    }

}
