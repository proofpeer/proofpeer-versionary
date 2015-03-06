package proofpeer.versionary

import java.util.zip._
import java.io._
import proofpeer.general.Bytes
import proofpeer.general.StringUtils

trait ArchiveCreator {

  def addContent(path : Seq[String], content : Bytes)

  def addDirectory(path : Seq[String])

  def close() : Bytes

}

trait ArchiveExtractor {

  def listFilesAndDirectories(f : (Seq[String], Option[Bytes]) => Unit) 

}

final class ZipArchiveCreator() extends ArchiveCreator {

  private val byteArrayOutputStream : ByteArrayOutputStream = new ByteArrayOutputStream()
  private val zipOutputStream : ZipOutputStream = new ZipOutputStream(byteArrayOutputStream)

  private def pathToString(path : Seq[String], directory : Boolean) : String = {
    val builder = new StringBuilder()
    var first : Boolean = true
    for (name <- path) {
      if (first) first = false else builder.append("/")
      builder.append(name)
    }
    if (directory) builder.append("/")
    builder.toString
  }

  def addContent(path : Seq[String], content : Bytes) {
    val e = new ZipEntry(pathToString(path, false))
    val len = content.length
    e.setSize(len)
    zipOutputStream.putNextEntry(e)
    zipOutputStream.write(content.toArray, 0, len)
  }

  def addDirectory(path : Seq[String]) {
    val e = new ZipEntry(pathToString(path, true))
    e.setSize(0)
    zipOutputStream.putNextEntry(e)
  }

  def close() : Bytes = {
    zipOutputStream.close()
    Bytes(byteArrayOutputStream.toByteArray)
  }

}

final class ZipArchiveExtractor(repo : Repository, bytes : Bytes) extends ArchiveExtractor {

  def listFilesAndDirectories(callback : (Seq[String], Option[Bytes]) => Unit) {
    val byteArrayInputStream : ByteArrayInputStream = new ByteArrayInputStream(bytes.toArray)
    val zipInputStream : ZipInputStream = new ZipInputStream(byteArrayInputStream)
    var zipEntry : ZipEntry = zipInputStream.getNextEntry()
    var buffer : Array[Byte] = new Array(2048)
    while (zipEntry != null) {
      val name = zipEntry.getName()
      val isDirectory = name.endsWith("/")
      val path = StringUtils.split_nonempty(name, "/").toList
      if (isDirectory) {
        if (repo.isValidDirectoryPath(path))
          callback(path, None)
      } else if (repo.checkValidContentPath(path).isDefined) {
        val outstream = new ByteArrayOutputStream()
        var len = 0
        do {
          len = zipInputStream.read(buffer)
          if (len > 0) outstream.write(buffer, 0, len)
        } while (len > 0)
        outstream.close()
        val bytes = Bytes(outstream.toByteArray)
        callback(path, Some(bytes))
      }
      zipEntry = zipInputStream.getNextEntry()
    }
    zipInputStream.close()
  } 

}