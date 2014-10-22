package proofpeer.versionary

object VersionaryImpl extends VersionaryCore {
  
  def repository : Repository = Repository.default

  def core : VersionaryCore = this

  def lookupBranch(branch : String) : Option[Branch] = {
    throw new RuntimeException("not implemented yet")
  }

  def lookupVersion(branch : String, version : Int) : Option[Version] = {
    throw new RuntimeException("not implemented yet")
  }

  def createNewBranch(branch : String, master : Option[(String, Int)], isPublic : Boolean, 
    login : Option[String], directory : DirectoryPointer) : Either[(Branch, Version), Branch] = 
  {
    throw new RuntimeException("not implemented yet")
  }

  def createNewVersion(branch : Branch, login : Option[String], importance : Int, comment : String, 
    directory : DirectoryPointer, parentVersion : Int, masterVersion : Int, timestamp : Timestamp, isEnabled : Boolean) : 
    Either[(Branch, Version), Branch] = 
  {
    throw new RuntimeException("not implemented yet")
  }

  def branchesOfLogin(login : String) : Vector[String] = {
    throw new RuntimeException("not implemented yet")
  }

}