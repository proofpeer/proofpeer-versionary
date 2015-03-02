package proofpeer.versionary

import java.util.zip._
import java.io._
import proofpeer.general.Bytes

trait ArchiveCreator {

  def addContent(path : Seq[String], content : Bytes)

  def addDirectory(path : Seq[String])

  def close() : Bytes

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