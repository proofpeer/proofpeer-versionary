package proofpeer.versionary

import proofpeer.general.Bytes

case class ContentPointerImpl(storageKey : Bytes, countConflicts : Int, contentTypeId : Int) extends ContentPointer {

  def kind = Value.CONTENT_KIND

  def toBytes = Bytes.encode((kind, storageKey, countConflicts, contentTypeId))  

}

case class ContentImpl(pointer : ContentPointer, get : Any) extends Content

case class DirectoryPointerImpl(storageKey : Bytes, countConflicts : Int, numEntries : Int) extends DirectoryPointer {

  def kind = Value.DIRECTORY_KIND

  def toBytes = Bytes.encode((kind, storageKey, countConflicts, numEntries)) 

}

case class DirectoryImpl(pointer : DirectoryPointer, entries : Vector[(String, ValuePointer)]) extends Directory 

case class ConflictPointerImpl(master : Option[ValuePointer], topic : Option[ValuePointer]) extends ConflictPointer with Conflict
{

  def countConflicts = 1

  def kind = Value.CONFLICT_KIND

  def toBytes = Bytes.encode((kind, master.map(_.toBytes), topic.map(_.toBytes)))

  def pointer = this
} 

class RepositoryImpl(storage : Storage) extends Repository {

  def pointerFromBytes(bytes : Bytes) : ValuePointer = {
    bytes.decode match {
      case Vector(Value.CONTENT_KIND, storageKey : Bytes, countConflicts : Long, contentTypeId : Long) =>
        ContentPointerImpl(storageKey, countConflicts.toInt, contentTypeId.toInt) 
      case Vector(Value.CONFLICT_KIND, master : Vector[Any], topic : Vector[Any]) =>
        def conv(vec : Vector[Any]) : Option[ValuePointer] = {
          vec match {
            case Vector() => None
            case Vector(b : Bytes) => Some(pointerFromBytes(b))
            case _ => throw new RuntimeException("invalid conflict pointer")
          }
        }
        ConflictPointerImpl(conv(master), conv(topic))
      case Vector(Value.DIRECTORY_KIND, storageKey : Bytes, countConflicts : Long, numEntries : Long) =>
        DirectoryPointerImpl(storageKey, countConflicts.toInt, numEntries.toInt)
      case _ => throw new RuntimeException("invalid pointer")
    }
  }

  def loadValue(pointer : ValuePointer) : Value = {
    pointer match {
      case pointer : ConflictPointerImpl => pointer
      case pointer : DirectoryPointerImpl => 
        val storedEntries = storage.load(pointer.storageKey).get.decode.asInstanceOf[Vector[Any]]
        def decodeEntry(storedEntry : Any) : (String, ValuePointer) = {
          storedEntry match {
            case Vector(name : String, encodedPointer : Bytes) =>
              (name, pointerFromBytes(encodedPointer))
            case _ => throw new RuntimeException("cannot decode directory entry")
          }
        }
        val entries = storedEntries.map(decodeEntry _)
        DirectoryImpl(pointer, entries)
      case pointer : ContentPointerImpl =>
        val contentBytes = storage.load(pointer.storageKey).get
        val contentType = ContentTypes.contentTypeOf(pointer.contentTypeId)
        val content = contentType.fromBytes(contentBytes)
        ContentImpl(pointer, content)
      case _ => throw new RuntimeException("invalid pointer: " + pointer)
    }
  }

  def createConflict(master : Option[ValuePointer], topic : Option[ValuePointer]) : Conflict = {
    ConflictPointerImpl(master, topic)
  }

  def createDirectory(entries : Vector[(String, ValuePointer)]) : Directory = {
    val value = Bytes.encode(entries.map({case (s : String, v : ValuePointer) => (s, v.toBytes)}))
    val storageKey = storage.save(value)
    var countConflicts = 0
    for ((_, p) <- entries) countConflicts += p.countConflicts
    val pointer = DirectoryPointerImpl(storageKey, countConflicts, entries.size)
    DirectoryImpl(pointer, entries)
  } 

  def createContent(contentTypeId : Int, content : Any) : Content = {
    val contentType = ContentTypes.contentTypeOf(contentTypeId)
    val value = contentType.toBytes(content)
    val storageKey = storage.save(value)
    val countConflicts = contentType.countConflicts(content)
    val pointer = ContentPointerImpl(storageKey, countConflicts, contentTypeId)
    ContentImpl(pointer, content)
  }

}






