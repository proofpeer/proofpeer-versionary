package proofpeer.versionary

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

object PathTests extends Properties("Path") {

  property("FilePathParsing1") = 
    FilePath("\\hello\\world") == new FilePath(true, Vector("hello", "world")) 
  property("FilePathParsing2") = 
    FilePath("\\hello\\world\\") == new FilePath(true, Vector("hello", "world"))
  property("FilePathParsing3") = 
    FilePath("hello\\world") == new FilePath(false, Vector("hello", "world"))
  property("FilePathParsing4") = 
    FilePath("hello\\world\\") == new FilePath(false, Vector("hello", "world"))
  property("FilePathParsing5") = 
    FilePath("") == new FilePath(false, Vector())
  property("FilePathParsing6") = 
    FilePath("\\") == new FilePath(true, Vector())
  property("FilePathParsing7") = 
    FilePath("\\peers\\obua\\PartizanGames.thy") == new FilePath(true, Vector("peers", "obua", "PartizanGames.thy"))


  private val parent = FilePath("\\peers\\obua")

  property("FilePathResolve1") = 
    FilePath("PartizanGames.thy").resolve(parent) == Some(FilePath("\\peers\\obua\\PartizanGames.thy"))

  property("FilePathResolve2") = 
    FilePath("\\PartizanGames.thy").resolve(parent) == Some(FilePath("\\PartizanGames.thy"))

  property("FilePathResolve3") = 
    FilePath("\\peers\\.\\..\\PartizanGames.thy").resolve(parent) == Some(FilePath("\\PartizanGames.thy"))

  property("FilePathResolve4") = 
    FilePath(".\\..\\PartizanGames.thy").resolve(parent) == Some(FilePath("\\peers\\PartizanGames.thy"))

  property("FilePathResolve5") = 
    FilePath(".\\..\\..").resolve(parent) == Some(FilePath("\\"))

  property("FilePathResolve6") = 
    FilePath(".\\..\\..\\..").resolve(parent) == None

  property("FilePathResolve7") = 
    FilePath(".\\..\\..\\..\\peers").resolve(parent) == None


}