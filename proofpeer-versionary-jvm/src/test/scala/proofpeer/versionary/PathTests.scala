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

  property("BranchParsing1") =
    PathGrammar.parseBranchSpec("obua\\hello:-4") == Some(BranchSpec(Some("obua\\hello"), Some(-4), None))
  property("BranchParsing2") =
    PathGrammar.parseBranchSpec("obua\\hello:9") == Some(BranchSpec(Some("obua\\hello"), Some(9), None))
  property("BranchParsing3") =
    PathGrammar.parseBranchSpec("obua\\hello") == Some(BranchSpec(Some("obua\\hello"), None, None))
  property("BranchParsing4") =
    PathGrammar.parseBranchSpec("obua\\hello:-4@local") == Some(BranchSpec(Some("obua\\hello"), Some(-4), Some("local")))
  property("BranchParsing5") =
    PathGrammar.parseBranchSpec("obua\\hello:9@proofpeer.net") == Some(BranchSpec(Some("obua\\hello"), Some(9), Some("proofpeer.net")))
  property("BranchParsing6") =
    PathGrammar.parseBranchSpec("obua\\hello@proofpeer.net") == Some(BranchSpec(Some("obua\\hello"), None, Some("proofpeer.net")))
  property("BranchParsing7") =
    PathGrammar.parseBranchSpec("") == Some(BranchSpec(None, None, None))
  property("BranchParsing8") =
    PathGrammar.parseBranchSpec(":-10") == Some(BranchSpec(None, Some(-10), None))
  property("BranchParsing9") =
    PathGrammar.parseBranchSpec("@local") == Some(BranchSpec(None, None, Some("local")))


  property("PathParsing1") = 
    Path("\\hello\\world") == Path(None, FilePath("\\hello\\world"))

  property("PathParsing2") = 
    Path("\\hello\\world@obua\\analysis") == Path(Some(BranchSpec(Some("obua\\analysis"), None, None)), FilePath("\\hello\\world"))

  property("PathParsing3") = 
    Path("\\hello\\world@obua\\hello:-001@proofpeer.net") == Path(Some(BranchSpec(Some("obua\\hello"), Some(-1), Some("proofpeer.net"))), 
      FilePath("\\hello\\world"))

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