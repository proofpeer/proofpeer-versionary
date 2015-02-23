package proofpeer.versionary

import proofpeer.general.Bytes

class StorageException(val reason : String) extends RuntimeException

/** Storage abstracts a key/value store on top of which Versionary can run. */
trait Storage {

  /** Saves a value to the store and returns the key for the value.
    * Multiple saves of the same value will return the same key.
    * If an error occurred a StorageException is thrown. 
    */
  def save(value : Bytes) : Bytes

  /** Returns the value that corresponds to the key if present in the store.
    * Returns None if no such key is associated with a value in the store.
    * Throws a StorageException if an error occurred.
    */
  def load(key : Bytes) : Option[Bytes]

}

object Storage {

  def apply() : Storage = {
    StorageImpl.get
  }  

}