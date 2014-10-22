package proofpeer.versionary

object VersionaryImpl {
  
  val core : VersionaryCore = new GAEVersionaryCore(Repository.default, "VersionaryBranches", "VersionaryVersions")

}