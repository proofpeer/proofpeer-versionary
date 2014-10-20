package proofpeer.versionary

import proofpeer.general.Bytes

object ContentTypes {
  val TEXT = 1

  def contentTypeOf(contentTypeId : Int) : ContentType = {
    contentTypeId match {
      case ContentTypes.TEXT => TextContentType
      case _ => throw new RuntimeException("unknown content type: " + contentTypeId) 
    }
  }  
}

object TextContentType extends ContentType {
  import proofpeer.general.StringUtils._

  def contentTypeId = ContentTypes.TEXT

  def fromBytes(bytes : Bytes) : Any = {
    bytes.asString
  }

  def toBytes(content : Any) : Bytes = {
    Bytes.fromString(content.asInstanceOf[String])
  }

  // These three functions will be implemented properly later.
  def countConflicts(content : Any) = 0
  def merge2way(content1 : Any, content2 : Any) : Option[Any] = None
  def merge3way(content1 : Any, content2 : Any) : Option[Any] = None  
}
