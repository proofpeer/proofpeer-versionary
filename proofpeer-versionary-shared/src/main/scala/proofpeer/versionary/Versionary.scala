package proofpeer.versionary

object Importance {

  /** The version has been created automatically by the system. */
  val AUTOMATIC = 1000

  /** The version is a pull. */
  val PULL = 2000

  /** The version is a sync. */
  val SYNC = 3000

  /** The version has been explicitly created by the peer. */
  val EXPLICIT = 4000

  /** The version is a milestone. */
  val MILESTONE = 5000

}

trait Version {

  /** The name of the branch this version belongs to. */
  def branch : String

  /** The id of this version. Valid ids are positive, i.e. the smallest valid id is 1. */
  def version : Int

  /** The login of the peer who created this version. 
    * If login is None then the system created this version. 
    */
  def login : Option[String]

  /** An integer denoting the importance of this version. All possible values are listed as
    * members of the [[Importance]] object.
    */
  def importance : Int

  /** A comment that describes the version. */
  def comment : String

  /** A timestamp describing when the version has been created. */
  def timestamp : Timestamp

  /** The directory snapshot that belongs to this version. */
  def directory : DirectoryPointer

  /** The id of the parent version, -1 if there is no parent. */
  def parentVersion : Int

  /** The id of the most recent master version that has been merged into this version, -1 if there is no such version. */
  def masterVersion : Int

  /** True if this version is enabled, false if it is disabled. Versions cannot be deleted, but they can be disabled. */
  def isEnabled : Boolean

}

trait Branch {

  /** The name of this branch. */
  def name : String

  /** The name of the master branch, if there is one. */
  def master : Option[String]

  /** True if this branch is enabled, false if it is disabled. Branches cannot be deleted, but they can be disabled. */
  def isEnabled : Boolean

  /** True if this branch is a public branch, i.e. visible to everyone. */
  def isPublic : Boolean

  /** The login of the peer who owns this branch, None if it is owned by the system. */
  def login : Option[String]

  /** The id of the current version that the branch is at. */
  def currentVersion : Int

  /** Timestamp of when this branch has been created. */
  def timestampCreated : Timestamp

  /** Timestamp of when this branch has been last modified. */
  def timestampModified : Timestamp

}

trait VersionaryCore {

  /** The repository that this versionary operates on. */
  def repository : Repository

  /** Returns the branch corresponding to the given name if such a branch exists in this Versionary. */
  def lookupBranch(branch : String) : Option[Branch]

  /** Returns the version if such a version exists. Note that version cannot be a relative (i.e. non-positive) version. */
  def lookupVersion(branch : String, version : Int) : Option[Version]

  /** Creates a new branch together with its first version.  
    * @param branch the name of the new branch
    * @param master points to the master branch and the master version that this new branch is created from
    * @param isPublic whether the branch is public or not
    * @param login the owner of the branch, None if this branch belongs to the system
    * @param directory the directory snapshot that the created version refers to
    * @return if the branch already exists, Right(branch) is returned; otherwise Left((branch, version)) is returned,
    *    where branch is the new branch and version the first version of the new branch.
    */
  def createNewBranch(branch : String, master : Option[(String, Int)], isPublic : Boolean, 
    login : Option[String], directory : DirectoryPointer) : Either[(Branch, Version), Branch]

  /** Creates a new version. The id of the new version will be branch.currentVersion + 1. 
    * @return If the new version cannot be created because the branch has already aquired other new versions, Right(branch)
    *   is returned where branch is the up-to-date branch object. Otherwise Left((updatedBranch, version)) is returned,
    *   where version.version == updatedBranch.currentVersion == branch.currentVersion + 1.
    */
  def createNewVersion(branch : Branch, login : Option[String], importance : Int, comment : String, 
    directory : DirectoryPointer, parentVersion : Int, masterVersion : Int, timestamp : Timestamp, isEnabled : Boolean) : 
    Either[(Branch, Version), Branch]

  /** Returns the names of all branches in this Versionary owned by the given peer. */
  def branchesOfLogin(login : String) : Vector[String]

}

trait Versionary extends VersionaryCore {
}

object Versionary {

  def normalizeBranchname(branch : String) : String = branch.toLowerCase

  def core : VersionaryCore = {
    VersionaryImpl.core
  }  

  private val versionary = new VersionaryExt(core)

  def apply() : Versionary = versionary

}

