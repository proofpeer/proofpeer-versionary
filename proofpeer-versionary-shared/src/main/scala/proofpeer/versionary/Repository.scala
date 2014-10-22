package proofpeer.versionary

import proofpeer.general.Bytes

object Value {
  val CONTENT_KIND = 1
  val CONFLICT_KIND = 2
  val DIRECTORY_KIND = 3
}

trait ValuePointer {
  def kind : Int
  def toBytes : Bytes
  def countConflicts : Int
}

trait ContentPointer extends ValuePointer {
  def contentTypeId : Int
  def storageKey : Bytes
}

trait ConflictPointer extends ValuePointer {
  def master : Option[ValuePointer]
  def topic : Option[ValuePointer]
}

trait DirectoryPointer extends ValuePointer {
  def numEntries : Int
  def storageKey : Bytes
}

trait Value {
  def pointer : ValuePointer
}

trait Content extends Value {
  def pointer : ContentPointer 
  def get : Any
}

trait Conflict extends Value {
  def pointer : ConflictPointer
}

trait Directory extends Value {
  def pointer : DirectoryPointer
  def entries : Vector[(String, ValuePointer)] 
}

/** Abstraction for a type of content.
  * Knows how to convert bytes to content and vice versa.
  * Also knows how to count conflicts in the content, and how to merge content. 
  */
trait ContentType {
  def contentTypeId : Int 
  def fromBytes(bytes : Bytes) : Any
  def toBytes(content : Any) : Bytes
  def countConflicts(content : Any) : Int
  def merge2way(content1 : Any, content2 : Any) : Option[Any]
  def merge3way(content1 : Any, content2 : Any) : Option[Any]
}

trait Repository {
  def pointerFromBytes(bytes : Bytes) : ValuePointer
  def loadValue(pointer : ValuePointer) : Value
  def createConflict(master : Option[ValuePointer], topic : Option[ValuePointer]) : Conflict
  def createDirectory(entries : Vector[(String, ValuePointer)]) : Directory
  def createContent(contentTypeId : Int, content : Any) : Content
}

object Repository {

  def apply(storage : Storage) : Repository = {
    new RepositoryImpl(storage)
  }

  val default : Repository = apply(Storage())

}

