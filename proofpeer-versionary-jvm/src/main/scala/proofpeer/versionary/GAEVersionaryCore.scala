package proofpeer.versionary

import com.google.appengine.api.datastore._
import GAETools._
import proofpeer.general.Bytes
import Versionary.normalizeBranchname

case class BranchImpl(name : String, master : Option[String], isEnabled : Boolean, isPublic : Boolean, 
  login : Option[String], currentVersion : Int, timestampCreated : Timestamp, timestampModified : Timestamp) extends Branch

case class VersionImpl(branch : String, version : Int, login : Option[String], importance : Int, 
  comment : String, timestamp : Timestamp, directory : DirectoryPointer, parentVersion : Int, masterVersion : Int, 
  isEnabled : Boolean) extends Version

class GAEVersionaryCore(val repository : Repository, branchKind : String, versionKind : String) extends VersionaryCore {

  private def datastoreService() : DatastoreService = DatastoreServiceFactory.getDatastoreService()

  private def optString(s : String) : Option[String] = 
    if (s == null) None else Some(s)

  private def branchKey(normalizedBranchname : String) : Key = 
    KeyFactory.createKey(branchKind, normalizedBranchname)

  private def versionKey(normalizedBranchname : String, version : Int) : Key = 
    KeyFactory.createKey(branchKey(normalizedBranchname), versionKind, normalizedBranchname + ":" + version)

  private def lookupBranchAndEntity(branch : String) : Option[(Branch, Entity)] = {
    val key = branchKey(normalizeBranchname(branch))
    get(datastoreService(), key) match {
      case None => None
      case Some(entity) => 
        def prop[T](name : String) = entity.getProperty(name).asInstanceOf[T]
        val name : String = prop("name")
        val master : String = prop("master")
        val isEnabled : Boolean = prop("isEnabled")
        val isPublic : Boolean = prop("isPublic")
        val login : String = prop("login")
        val currentVersion : Int = prop[Long]("currentVersion").toInt
        val timestampCreated : Long = prop("timestampCreated")
        val timestampModified : Long = prop("timestampModified")
        Some((BranchImpl(name, optString(master), isEnabled, isPublic, optString(login), currentVersion,
          Timestamp(timestampCreated), Timestamp(timestampModified)), entity))
    }
  }

  def lookupBranch(branch : String) : Option[Branch] = 
    lookupBranchAndEntity(branch).map(_._1)

  def lookupVersion(branch : String, version : Int) : Option[Version] = {
    val key = versionKey(normalizeBranchname(branch), version)
    get(datastoreService(), key) match {
      case None => None
      case Some(entity) => 
        def prop[T](name : String) = entity.getProperty(name).asInstanceOf[T]
        val branch : String = prop("branch")
        val version : Int = prop[Long]("version").toInt
        val login : String = prop("login")
        val importance : Int = prop[Long]("importance").toInt
        val comment : String = prop("comment")
        val timestamp : Long = prop("timestamp")
        val directory : ShortBlob = prop("directory")
        val parentVersion : Int = prop[Long]("parentVersion").toInt
        val masterVersion : Int = prop[Long]("masterVersion").toInt
        val isEnabled : Boolean = prop("isEnabled")
        val directoryPointer = repository.pointerFromBytes(Bytes(directory.getBytes())).asInstanceOf[DirectoryPointer]
        Some(VersionImpl(branch, version, optString(login), importance, comment, Timestamp(timestamp), 
          directoryPointer, parentVersion, masterVersion, isEnabled))
    }
  }

  def createNewBranch(branch : String, master : Option[(String, Int)], isPublic : Boolean, 
    login : Option[String], directory : DirectoryPointer) : Either[(Branch, Version), Branch] = 
  {
    val nbranch = normalizeBranchname(branch)
    val bKey = branchKey(nbranch)
    val vKey = versionKey(nbranch, 1)
    val datastore = datastoreService()
    var result : Either[(Branch, Version), Branch] = null
    atomic(datastore) {
      lookupBranch(branch) match {
        case Some(branch) => 
          result = Right(branch)
        case None =>
          val bEntity = new Entity(bKey)
          def bprop(name : String, value : Any) { bEntity.setProperty(name, value) }
          bprop("name", branch)
          if (master.isDefined) bprop("master", master.get._1) else bprop("master", null)
          val branchIsEnabled = true
          bprop("isEnabled", branchIsEnabled)
          bprop("isPublic", isPublic)
          bprop("login", if (login.isDefined) login.get else null)
          val version = 1
          bprop("currentVersion", version)
          val now = Timestamp.now
          val nowMillis = now.toMillis
          bprop("timestampCreated", nowMillis)
          bprop("timestampModified", nowMillis)
          datastore.put(bEntity)
          val vEntity = new Entity(vKey)
          def vprop(name : String, value : Any) { vEntity.setProperty(name, value) }
          vprop("branch", nbranch)
          vprop("version", version)
          vprop("login", if (login.isDefined) login.get else null)
          val importance = Importance.AUTOMATIC
          vprop("importance", importance)
          val comment = "Initial version of branch."
          vprop("comment", comment)
          vprop("timestamp", nowMillis)
          vprop("directory", new ShortBlob(directory.toBytes.toArray))
          val parentVersion = -1
          vprop("parentVersion", parentVersion)
          val masterVersion = if (master.isDefined) master.get._2 else -1
          vprop("masterVersion", masterVersion)
          val versionIsEnabled = true
          vprop("isEnabled", versionIsEnabled)
          datastore.put(vEntity)
          val branchImpl = BranchImpl(branch, master.map(_._1), branchIsEnabled, isPublic, login, version, now, now)
          val versionImpl = VersionImpl(nbranch, version, login, importance, comment, now, directory, parentVersion,
            masterVersion, versionIsEnabled)
          result = Left((branchImpl, versionImpl))
      }
    }
    result
  }

  def createNewVersion(branch : Branch, login : Option[String], importance : Int, comment : String, 
    directory : DirectoryPointer, parentVersion : Int, masterVersion : Int, timestamp : Timestamp, isEnabled : Boolean) : 
    Either[(Branch, Version), Branch] = 
  {
    val datastore = datastoreService()
    var result : Either[(Branch, Version), Branch] = null
    atomic(datastore) {
      lookupBranchAndEntity(branch.name) match {
        case None =>
          throw new RuntimeException("no such branch found: " + branch.name)
        case Some((currentBranch, bEntity)) => 
          if (currentBranch.currentVersion != branch.currentVersion)
            result = Right(currentBranch)
          else {
            val version = currentBranch.currentVersion + 1
            bEntity.setProperty("currentVersion", version)
            val now = Timestamp.now
            bEntity.setProperty("timestampModified", now.toMillis)
            datastore.put(bEntity)
            val nbranch = normalizeBranchname(branch.name)
            val vKey = versionKey(nbranch, version)
            val vEntity = new Entity(vKey)
            def vprop(name : String, value : Any) { vEntity.setProperty(name, value) }
            vprop("branch", nbranch)
            vprop("version", version)
            vprop("login", if (login.isDefined) login.get else null)
            vprop("importance", importance)
            vprop("comment", comment)
            vprop("timestamp", timestamp.toMillis)
            vprop("directory", new ShortBlob(directory.toBytes.toArray))
            vprop("parentVersion", parentVersion)
            vprop("masterVersion", masterVersion)
            vprop("isEnabled", isEnabled)
            datastore.put(vEntity)
            val b = currentBranch
            val branchImpl = BranchImpl(b.name, b.master, b.isEnabled, b.isPublic, b.login, version,
              b.timestampCreated, now)
            val versionImpl = VersionImpl(nbranch, version, login, importance, comment, timestamp, directory, parentVersion,
              masterVersion, isEnabled)
            result = Left((branchImpl, versionImpl))
          }
      }
    }
    result
  }

  def branchesOfLogin(login : String) : Vector[String] = {
    val datastore = datastoreService()
    val q = query(branchKind, equal($("login"), login))
    q.addProjection(new PropertyProjection("name", classOf[String]));
    val names = 
      for (b <- fetch(datastore, q, Int.MaxValue)) 
        yield b.getProperty("name").asInstanceOf[String]
    names.toVector
  }

} 