package proofpeer.versionary

import com.google.appengine.api.datastore._
import proofpeer.general.Bytes

class GAEStorage(kind : String) extends Storage {

  def save(value : Bytes) : Bytes = {
    try {
      val binaryKey = value.sha256
      val datastore = DatastoreServiceFactory.getDatastoreService()
      val key = KeyFactory.createKey(kind, binaryKey.asHex)
      GAETools.get(datastore, key) match {
        case None =>
          val entity = new Entity(key)
          entity.setProperty("value", new Blob(value.toArray))
          datastore.put(entity)
          binaryKey
        case Some(entity) =>
          val oldValue = Bytes(entity.getProperty("value").asInstanceOf[Blob].getBytes())
          if (oldValue != value) throw new StorageException("SHA256 collision")
          binaryKey
      }    
    } catch {
      case x : Throwable => throw new StorageException(x.getMessage)      
    }
  }

  def load(key : Bytes) : Option[Bytes] = {
    try {
      val datastore = DatastoreServiceFactory.getDatastoreService()
      val entityKey = KeyFactory.createKey(kind, key.asHex)
      GAETools.get(datastore, entityKey) match {
        case None => None
        case Some(entity) => Some(Bytes(entity.getProperty("value").asInstanceOf[Blob].getBytes()))
      }
    } catch {
      case x : Throwable => throw new StorageException(x.getMessage)
    }
  }

}
