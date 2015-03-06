package proofpeer.versionary

import proofpeer.indent._
import proofpeer.indent.regex._
import proofpeer.indent.Constraint._

object PathGrammar {

  val letter = alt(chars('a', 'z'), chars('A', 'Z'))
  val digit = chars('0', '9')
  val underscore = char('_')
  val alphanum = alt(letter, digit, underscore)
  val suffix = OPT(seq(string("."), REPEAT1(alphanum)))
  val id = seq(letter, REPEAT(alphanum), suffix)

  val grammar = 
    rule("Filename", alt(string("."), string(".."), id)) ++
    rule("PathSeparator", char('\\')) ++
    rule("Colon", char(':')) ++
    rule("At", char('@')) ++
    rule("BranchId", seq(id, REPEAT(seq(char('\\'), id)))) ++
    rule("Domain", seq(REPEAT1(letter), OPT(seq(char('.'), REPEAT1(letter))))) ++
    rule("Version", seq(OPT(char('-')), REPEAT1(digit))) ++
    rule("RelativeFilePath", "", c => Vector[String]()) ++
    rule("RelativeFilePath", "Filename", c => Vector[String](c.text("Filename"))) ++
    rule("RelativeFilePath", "Filename PathSeparator RelativeFilePath",
      connect("Filename", "PathSeparator", "RelativeFilePath"), 
      c =>  c.text("Filename") +: c.RelativeFilePath[Vector[String]]) ++
    rule("FilenameNonterminal", "Filename", c => ()) ++
    rule("FilePath", "RelativeFilePath", c => FilePath(false, c.RelativeFilePath[Vector[String]])) ++
    rule("FilePath", "PathSeparator RelativeFilePath", Connect("PathSeparator", "RelativeFilePath"), 
      c => FilePath(true, c.RelativeFilePath[Vector[String]])) ++
    rule("OptBranchId", "BranchId", c => Some(c.text("BranchId"))) ++
    rule("OptBranchId", "", c => None) ++
    rule("Branch", "OptBranchId", c => BranchSpec(c.OptBranchId, None, None)) ++
    rule("Branch", "OptBranchId Colon Version",
      connect("OptBranchId", "Colon", "Version"),
      c => BranchSpec(c.OptBranchId, Some(c.text("Version").toInt), None)) ++
    rule("BranchSpec", "Branch", c => c.Branch[Any]) ++
    rule("BranchSpec", "Branch At Domain",
      connect("Branch", "At", "Domain"),
      c => c.Branch[BranchSpec].setDomain(c.text("Domain"))) ++ 
    rule("Path", "FilePath", c => Path(None, c.FilePath)) ++
    rule("Path", "FilePath At BranchSpec", 
      connect("FilePath", "At", "BranchSpec"),
      c => Path(Some(c.BranchSpec), c.FilePath))

  val parser = Parser(grammar)

  def parseFilePath(filepath : String) : Option[FilePath] = parser.parse("FilePath", filepath)
  def parseBranchSpec(branchspec : String) : Option[BranchSpec] = parser.parse("BranchSpec", branchspec)
  def parsePath(path : String) : Option[Path] = parser.parse("Path", path)

  def isFilename(filename : String) : Boolean = parser.parse[Unit]("FilenameNonterminal", filename).isDefined

}

case class FilePath(val absolute : Boolean, val path : Vector[String]) {
  override def toString = 
    if (absolute) "\\" + path.mkString("\\") else if (path.isEmpty) "." else path.mkString("\\")
  lazy val isResolved = absolute && !path.exists(s => s == "." || s == "..")
  
  /** Resolves this path relative to parentFilePath which must be resolved. */
  def resolve(parentFilePath : FilePath) : Option[FilePath] = {
    if (!parentFilePath.isResolved) throw new RuntimeException("parentFilePath is not resolved")
    if (this.isResolved) return Some(this)
    val p = 
      if (absolute) resolvePath(List(), path.toList)
      else resolvePath(parentFilePath.path.reverse.toList, path.toList)
    if (p == null)
      None
    else 
      Some(FilePath(true, p.reverse.toVector))
  }

  /** Resolves the child relative to the reverseParent path. The result will be reversed, just like the parent. 
    * If the path cannot be resolved, null is returned.
    */
  private def resolvePath(reverseParent : List[String], child : List[String]) : List[String] = {
    child match {
      case "." :: child => 
        resolvePath(reverseParent, child)
      case ".." :: child => 
        if (reverseParent.isEmpty) null
        else resolvePath(reverseParent.tail, child)
      case c :: child =>
        resolvePath(c :: reverseParent, child) 
      case List() => 
        reverseParent
    }
  }

  def up : Option[FilePath] = {
    if (path.isEmpty) None 
    else Some(FilePath(absolute, path.take(path.size - 1)))
  }

  def down(name : String) : FilePath = {
    FilePath(absolute, path :+ name)
  }

  def lastName : Option[String] = {
    if (path.isEmpty) None else Some(path.last)
  }

}

object FilePath {

  def apply(path : String) : FilePath = {
    PathGrammar.parseFilePath(path) match {
      case None => throw new RuntimeException("invalid file path: " + path)
      case Some(fp) => fp
    }
  }

}

object FilePathOrdering extends Ordering[FilePath] {
  
  def compare(a:FilePath, b:FilePath) : Int = {
    if (a.absolute != b.absolute) {
      if (a.absolute) -1 else 1
    } else if (a.path.length != b.path.length) { 
      a.path.length - b.path.length
    } else {
      for (i <- 0 until a.path.length) {
        val c = compareFilenames(a.path(i), b.path(i))
        if (c != 0) return c
      }
      0
    }
  }

  def compareFilenames(a : String, b : String) : Int = {
    val x = a.toLowerCase
    val y = b.toLowerCase
    x compare y
  }

}

case class BranchSpec(name : Option[String], version : Option[Int], domain : Option[String]) {
  def setDomain(d : String) : BranchSpec = BranchSpec(name, version, Some(d))
  override def toString : String = {
    val n = if (name.isDefined) name.get else ""
    val v = if (version.isDefined) ":" + version.get else ""
    val d = if (domain.isDefined) "@" + domain.get else ""
    n + v + d
  }
  def resolve(reference : BranchSpec) : BranchSpec = {
    val n = if (name.isDefined) name else reference.name
    val iv = versionAsInt
    val v = 
      if (iv <= 0) {
        val v = reference.versionAsInt + iv
        if (v == 0) None else Some(v)
      } else version
    val d = if (domain.isDefined) domain else reference.domain
    BranchSpec(n, v, d)
  }
  def versionIsRelative : Boolean = versionAsInt <= 0
  def removeVersion : BranchSpec = BranchSpec(name, None, domain)
  def setVersion(version : Int) : BranchSpec = BranchSpec(name, Some(version), domain)
  def versionAsInt : Int = {
    version match {
      case None => 0
      case Some(v) => v
    }
  }
}

class Path(val branch : Option[BranchSpec], val path : FilePath) {
  def up : Option[Path] = {
    path.up match {
      case None => None
      case Some(path) => Some(new Path(branch, path))
    }
  }
  override def toString : String = {
    branch match {
      case None =>
        path.toString
      case Some(branch) =>
        path.toString + "@" + branch.toString
    }
  }
  def resolve(reference : Path) : Option[Path] = {
    if (!reference.isResolved) throw new RuntimeException("reference is not resolved: " + reference)
    path.resolve(reference.path) match {
      case None => None
      case Some(filepath) =>
        if (!branch.isDefined) Some(Path(reference.branch, filepath))
        else Some(Path(Some(branch.get.resolve(reference.branch.get)), filepath))
    }
  }
  lazy val isResolved : Boolean = {
    if (!path.isResolved || !branch.isDefined) false
    else {
      val b = branch.get
      b.name.isDefined && b.domain.isDefined// && !(b.version.isDefined && b.version.get < 0)
    }
  }
  def versionIsRelative : Boolean = 
    branch match {
      case None => true
      case Some(branch) => branch.versionIsRelative
    }
  def removeVersion : Path = 
    branch match {
      case None => this
      case Some(branch) => Path(Some(branch.removeVersion), path)
    }
  def setVersion(version : Int) : Path = {
    branch match {
      case None => Path(Some(BranchSpec(None, Some(version), None)), path)
      case Some(branch) => Path(Some(branch.setVersion(version)), path)
    }
  }
  def domain : Option[String] = {
    branch match {
      case None => None
      case Some(branchspec) => branchspec.domain
    }
  }
  def version : Option[Int] = {
    branch match {
      case None => None
      case Some(branchspec) => branchspec.version
    }
  }
  def branchname : Option[String] = {
    branch match {
      case None => None
      case Some(branchspec) => branchspec.name
    }
  }
  def pathnames : List[String] = path.path.toList
  private def rep : String = this.toString.toLowerCase
  override def hashCode() : Int = rep.hashCode
  override def equals(that : Any) : Boolean = {
    that match {
      case that : Path => rep.equals(that.rep)
      case _ => false
    }
  }
}

object Path {
  def apply(path : String) : Path = {
    PathGrammar.parsePath(path) match {
      case None => throw new RuntimeException("invalid path: " + path)
      case Some(p) => p
    }
  }
  def apply(branch : Option[BranchSpec], path : FilePath) : Path = {
    new Path(branch, path)
  }
}
