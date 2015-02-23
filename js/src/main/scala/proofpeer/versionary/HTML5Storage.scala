package proofpeer.versionary

import scala.scalajs.js
import proofpeer.general.Bytes

class HTML5Storage(keyPrefix : String) extends Storage {

  private val storage = js.Dynamic.global.window.localStorage

  private def put(key : String, value : String) {
    storage.setItem(keyPrefix + key, value)
  }

  private def get(key : String) : Option[String] = {
    val result = storage.getItem(keyPrefix + key)
    if (result == null) None else Some(result.asInstanceOf[String])
  }

  def save(value : Bytes) : Bytes = {
    try {
      val binaryKey = value.sha256
      val k = Bytes.encodeToUCS2(binaryKey)
      val v = Bytes.encodeToUCS2(value)
      put(k, v)
      binaryKey
    } catch {
      case x : Throwable => throw new StorageException(x.getMessage)
    }
  }

  def load(binaryKey : Bytes) : Option[Bytes] = {
    try {
      val k = Bytes.encodeToUCS2(binaryKey)
      get(k) match {
        case None => None
        case Some(ucs2) => Some(Bytes.decodeFromUCS2(ucs2).asInstanceOf[Bytes])
      }
    } catch {
      case x : Throwable => throw new StorageException(x.getMessage)      
    }
  }

}