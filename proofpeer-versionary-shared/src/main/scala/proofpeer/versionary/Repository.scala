package proofpeer.versionary

import proofpeer.general.Bytes

object Value {
  val CONTENT_KIND = 1
  val CONFLICT_KIND = 2
  val DIRECTORY_KIND = 3
}

sealed trait ValuePointer {
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

object Repository {

  def apply(storage : Storage) : Repository = {
    new RepositoryImpl(storage)
  }

  val default : Repository = apply(Storage())

}

trait Repository {
  def pointerFromBytes(bytes : Bytes) : ValuePointer
  def loadValue(pointer : ValuePointer) : Value
  def createConflict(master : Option[ValuePointer], topic : Option[ValuePointer]) : Conflict
  def createDirectory(entries : Vector[(String, ValuePointer)]) : Directory
  def createContent(contentTypeId : Int, content : Any) : Content

  /* -----------------
   *  Derived Methods 
   * ----------------- */

  def emptyDirectory : Directory = createDirectory(Vector())

  def loadDirectory(pointer : DirectoryPointer) : Directory =
    loadValue(pointer).asInstanceOf[Directory]

  def loadContent(pointer : ContentPointer) : Content =
    loadValue(pointer).asInstanceOf[Content]

  def loadConflict(pointer : ConflictPointer) : Conflict = 
    loadValue(pointer).asInstanceOf[Conflict]

  private def partialLookupPointer(root : ValuePointer, path : Seq[String], collected : List[String]) : 
    (Seq[String], ValuePointer, Seq[String]) = 
  {
    if (path.isEmpty)
      (collected.reverse, root, path)
    else {
      root match {
        case directoryPointer : DirectoryPointer =>
          val directory = loadDirectory(directoryPointer)
          val name = path.head
          directory.entries.find(entry => 
            FilePathOrdering.compareFilenames(entry._1, name) == 0
          ) match {
            case None => (collected.reverse, root, path)
            case Some((n, p)) => partialLookupPointer(p, path.tail, n::collected)
          }
        case _ => (collected.reverse, root, path)
      }
    }    
  }

  def partialLookupPointer(root : ValuePointer, path : Seq[String]) : 
    (Seq[String], ValuePointer, Seq[String]) =
  {
    partialLookupPointer(root, path, List())
  }

  def lookupPointer(root : ValuePointer, path : Seq[String]) : 
    Option[(Seq[String], ValuePointer)] = 
  {
    val (foundPath, foundPointer, leftPath) = partialLookupPointer(root, path)
    if (leftPath.isEmpty) Some((foundPath, foundPointer)) else None
  }

  /** Returns an integer 0 <= i <= entries.size such that 
    *  - entries(j) < filename for all j < i
    *  - filename < entries(j) for all i < j
    *  - filename <= entries(j) for all i = j
    * This assumes that entries is sorted and duplicate-free. 
    */
  def findPosition(entries : Vector[(String, ValuePointer)], filename : String) : Int = {
    var i = 0
    val len = entries.size
    while (i < len) {
      val c = FilePathOrdering.compareFilenames(entries(i)._1, filename)
      if (c >= 0) return i
      i = i + 1
    }
    return len
  }

  def findEntry(entries : Vector[(String, ValuePointer)], filename : String) : 
    (Int, Option[(String, ValuePointer)]) = 
  {
    val position = findPosition(entries, filename)
    if (position < entries.size && FilePathOrdering.compareFilenames(entries(position)._1, filename) == 0)
      (position, Some(entries(position)))
    else
      (position, None)
  }

  def mkdir(root : Directory, path : List[String]) : Option[(List[String], Directory)] = {
    if (path.isEmpty) Some((path, root))
    else {
      val filename = path.head
      findEntry(root.entries, filename) match {
        case (pos, None) =>
          mkdir(emptyDirectory, path.tail) match {
            case None => throw new RuntimeException("mkdir: internal error")
            case Some((createdPath, createdDirectory)) =>
              val entries = root.entries.take(pos) ++ Vector((filename, createdDirectory.pointer)) ++ root.entries.drop(pos)
              Some((filename::createdPath, createDirectory(entries)))
          }
        case (pos, Some((foundName, foundPointer))) =>
          foundPointer match {
            case directoryPointer : DirectoryPointer =>
              mkdir(loadDirectory(directoryPointer), path.tail) match {
                case None => None
                case Some((createdPath, createdDirectory)) =>
                  val entries = root.entries.take(pos) ++ Vector((foundName, createdDirectory.pointer)) ++ root.entries.drop(pos+1)
                  Some((foundName::createdPath, createDirectory(entries)))
              }
            case _ => None
          }
      }
    }
  }

}


