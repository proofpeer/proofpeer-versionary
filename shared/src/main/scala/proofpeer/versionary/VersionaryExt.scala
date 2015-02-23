package proofpeer.versionary

class VersionaryExt(core : VersionaryCore) extends Versionary {

  def repository : Repository = 
    core.repository

  def lookupBranch(branch : String) : Option[Branch] = 
    core.lookupBranch(branch)

  def lookupVersion(branch : String, version : Int) : Option[Version] =
    core.lookupVersion(branch, version)

  def createNewBranch(branch : String, master : Option[(String, Int)], isPublic : Boolean, 
    login : Option[String], directory : DirectoryPointer) : Either[(Branch, Version), Branch] =
    core.createNewBranch(branch, master, isPublic, login, directory)

  def createNewVersion(branch : Branch, login : Option[String], importance : Int, comment : String, 
    directory : DirectoryPointer, parentVersion : Int, masterVersion : Int, timestamp : Timestamp, isEnabled : Boolean) : 
    Either[(Branch, Version), Branch] =
    core.createNewVersion(branch, login, importance, comment, directory, parentVersion, masterVersion, timestamp, isEnabled)

  def branchesOfLogin(login : String) : Vector[String] =
    core.branchesOfLogin(login)

  def queryVersions(branch : String, importance : Int, timespan : TimeSpan, onlyEnabled : Boolean) : List[Version] =
    core.queryVersions(branch, importance, timespan, onlyEnabled)

}
