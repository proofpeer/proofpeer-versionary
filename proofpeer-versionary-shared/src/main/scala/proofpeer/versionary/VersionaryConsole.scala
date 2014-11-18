package proofpeer.versionary

object VersionaryConsole {

  def resolveCurrentPath(domain : String, path : String) : Either[Path, String] = {
    PathGrammar.parsePath(path) match {
      case None => Right("invalid path syntax")
      case Some(cp) =>
        if (!cp.isResolved) Right("path is unresolved")
        else if (cp.branch.get.domain.get != domain) Right("wrong domain")
        else Left(cp)
    }
  }
}

class VersionaryConsole(versionary : Versionary, login : Option[String], currentPath : Path, domain : String) {

  def INVALID_PATH[S] : Either[S, String] = Right("Invalid path.")

  def OUTDATED_VERSION[S](branch : Branch, version : Version) : Either[S, String] =
    Right("Version " + version.version + " is not the current version (" + 
      branch.currentVersion + ") of the branch.") 

  def resolve(path : String) : Option[Path] = {
    PathGrammar.parsePath(path) match {
      case None => None
      case Some(path) => path.resolve(currentPath)
    }
  }

  def loadVersion(path : Path) : Option[(Branch, Version)] = {
    val b = path.branch.get
    if (b.domain.get != domain) None
    else {
      versionary.lookupBranch(b.name.get) match {
        case None => None
        case Some(branch) =>
          val version = if (b.version.isDefined) b.version.get else 0
          val v = if (version <= 0) branch.currentVersion + version else version
          versionary.lookupVersion(b.name.get, v) match {
            case None => None
            case Some(version) => Some((branch, version))
          }
      }
    }
  }

  def loadVersion(path : String) : Option[(Branch, Version)] = {
    resolve(path) match {
      case None => None
      case Some(path) => loadVersion(path)
    }
  }

  def loadVersion(branch : String, version : Int) : Option[(Branch, Version)] = {
    val path = Path(Some(BranchSpec(Some(branch), Some(version), Some(domain))), new FilePath(true, Vector()))
    loadVersion(path)
  }

  def loadPath(path : Path) : Option[(Branch, Version, Option[(ValuePointer, Path)])] = {
    loadVersion(path) match {
      case None => None
      case Some((branch, version)) =>
        val branchspec = BranchSpec(Some(branch.name), Some(version.version), Some(domain))
        versionary.repository.lookupPointer(version.directory, path.path.path) match {
          case None => Some((branch, version, None))
          case Some((names, pointer)) =>
            val p = Path(Some(branchspec), FilePath(true, names.toVector))
            Some((branch, version, Some(pointer, p)))
        }
    }
  }

  def resolvePath(path : String) : Option[(Branch, Version, ValuePointer, Path)] = {
    resolve(path) match {
      case None => None
      case Some(path) => 
        loadPath(path) match {
          case Some((branch, version, Some((pointer, path)))) =>
            Some((branch, version, pointer, path))
          case _ => None
        }
    }
  }

  def describePointer(who : String, pointer : ValuePointer) : String = {
    val output = new StringBuilder()
    val conflicts = pointer.countConflicts
    pointer match {
      case pointer : ContentPointer =>
        val contentType = ContentTypes.contentTypeOf(pointer.contentTypeId)
        output ++= who + " is a " + contentType + " file"
        if (conflicts > 0) output ++= " with " + conflicts + " conflicts"
      case pointer : DirectoryPointer =>
        val entriesWord = if (pointer.numEntries == 1) " entry" else " entries"
        output ++= who + " is a directory with " + pointer.numEntries + entriesWord
        val conflictsWord = if (conflicts == 1) " conflict" else " conflicts"
        if (conflicts > 0) output ++= " and " + conflicts + conflictsWord
      case pointer : ConflictPointer =>
        output ++= who + " is a conflict"
    }
    output.toString()
  }

  def lsCmd(path : String) : Either[String, String] = {
    val separator = ":\n\n"
    resolvePath(path) match {
      case None => INVALID_PATH
      case Some((branch, version, valuepointer, p)) =>
        val output = new StringBuilder()
        output ++= describePointer(p.toString, valuepointer)
        valuepointer match {
          case pointer : ContentPointer =>
            if (pointer.kind == ContentTypes.PROOFSCRIPT) {
              val content = versionary.repository.loadContent(pointer).get
              output ++= separator
              output ++= content.toString
            }
            output ++= "\n"
          case pointer : ConflictPointer =>
            output ++= separator
            (pointer.master, pointer.topic) match {
              case (None, Some(topic)) => 
                output ++= "master: the entry does not exist\n"
                output ++= describePointer("topic:", topic)
                output ++= "\n"                
              case (Some(master), None) =>
                output ++= describePointer("master:", master)
                output ++= "\n"
                output ++= "topic: the entry does not exist"
                output ++= "\n"                
              case (Some(master), Some(topic)) =>
                output ++= describePointer("master:", master)
                output ++= "\n"
                output ++= describePointer("topic:", topic)
                output ++= "\n"                
              case (None, None) =>
                throw new RuntimeException("conflict encountered with neither master or topic")
            }
          case pointer : DirectoryPointer => 
            if (pointer.numEntries > 0) {
              output ++= separator
              val entries = versionary.repository.loadDirectory(pointer).entries
              for (i <- 0 until entries.size) {
                val (name, pointer) = entries(i)
                output ++= describePointer((i + 1).toString + ") " + name, pointer)
                output ++= "\n"
              }
            } else output ++= "\n"
        }
        output ++= "\n"
        Left(output.toString)        
    }
  }

  def pathCmd(path : String) : Either[String, String] = {
    resolve(path) match {
      case None => INVALID_PATH
      case Some(path) => Left(path.toString)
    }
  }

  def cdCmd(path : String) : Either[(String, String), String] = {
    resolve(path) match {
      case None => INVALID_PATH
      case Some(path) => 
        val versionIsRelative = path.versionIsRelative
        loadPath(path) match {
          case Some((branch, version, Some((pointer, p)))) => 
            pointer match {
              case _ : DirectoryPointer =>
                val path = 
                  if (!versionIsRelative || version.version != branch.currentVersion) 
                    p
                  else 
                    p.removeVersion
                Left((path.toString, "Switched to directory '" + path.toString + "'."))
              case _ => Right("No such directory.")
            }
          case _ => INVALID_PATH
        }
    }
  }

  def mkdirCmd(path : String) : Either[String, String] = {
    resolve(path) match {
      case None => INVALID_PATH
      case Some(path) =>
        loadPath(path) match {
          case Some((branch, version, None)) => 
            if (version.version != branch.currentVersion)
              OUTDATED_VERSION(branch, version)
            else {
              val r = versionary.repository
              r.mkdir(r.loadDirectory(version.directory), path.path.path.toList) match {
                case None => INVALID_PATH
                case Some((createdFilePath, createdDirectory)) =>
                  val comment = "Created directory '" + FilePath(true, createdFilePath.toVector) + "'."
                  versionary.createNewVersion(branch, login, Importance.AUTOMATIC, comment, 
                    createdDirectory.pointer, version.version, version.masterVersion, Timestamp.now, true) match 
                  {
                    case Left((newBranch, newVersion)) =>
                      val newPath = path.removeVersion
                      val output = "Created directory '" + newPath + "'."
                      Left(output) 
                    case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
                  }
              }
            }
          case Some(_) =>
            Right("The path already exists.")
          case None => INVALID_PATH
        }
    }
  }

  def rmCmd(path : String) : Either[String, String] = {
    resolvePath(path) match {
      case None => INVALID_PATH
      case Some((branch, version, valuepointer, path)) =>
        if (version.version != branch.currentVersion)
          OUTDATED_VERSION(branch, version)
        else {
          val r = versionary.repository
          r.rm(r.loadDirectory(version.directory), path.path.path.toList) match {
            case None => Right("Cannot delete toplevel directory.")
            case Some((deletedFilePath, updatedDirectory)) =>
              val comment = "Deleted '" + FilePath(true, deletedFilePath.toVector) + "'."
              versionary.createNewVersion(branch, login, Importance.AUTOMATIC, comment,
                updatedDirectory.pointer, version.version, version.masterVersion, Timestamp.now, true) match 
              {
                case Left((newBranch, newVersion)) =>
                  Left("Deleted object '" + path + "'. ")
                case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
              }
          }
        }
    }
  }

  def cpCmd(sourcePath : String, destPath : String) : Either[String, String] = {
    resolvePath(sourcePath) match {
      case None => Right("Invalid source path.")
      case Some((sourceBranch, sourceVersion, sourceValuepointer, sourcePath)) =>
        resolve(destPath) match {
          case None => Right("Invalid destination path.")
          case Some(destPath) => 
            loadVersion(destPath) match {
              case None => Right("Invalid destination path.")
              case Some((branch, version)) =>
                if (version.version != branch.currentVersion)
                  OUTDATED_VERSION(branch, version)
                else {
                  val r = versionary.repository
                  r.set(r.loadDirectory(version.directory), destPath.path.path.toList, sourceValuepointer) match {
                    case None => Right("Invalid destination directory.")
                    case Some((filePath, updatedDirectory)) =>
                      val comment = "Copied '" + sourcePath + "' to '" + FilePath(true, filePath.toVector) + "'."
                      versionary.createNewVersion(branch, login, Importance.AUTOMATIC, comment,
                        updatedDirectory.pointer, version.version, version.masterVersion, Timestamp.now, true) match 
                      {
                        case Left((newBranch, newVersion)) =>
                          Left("Copied '" + sourcePath + "' to '" + destPath.removeVersion + "'.")
                        case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
                      } 
                  }
                }
            }
        } 
    }
  }

  def history(version : Version, importance : Int, timespan : TimeSpan, onlyEnabled : Boolean) : List[Version] = {
    val tail : List[Version] = 
      if (version.parentVersion >= 0 && timespan.inPresentOrFuture(version.timestamp)) {
        val parentVersion = versionary.lookupVersion(version.branch, version.parentVersion)
        parentVersion match {
          case None => List()
          case Some(parentVersion) => history(parentVersion, importance, timespan, onlyEnabled)
        }
      } else List()
    if ((!onlyEnabled || version.isEnabled) && timespan.inPresent(version.timestamp) && 
        Versionary.matchesImportance(importance, version.importance))
      version :: tail
    else 
      tail
  }

  def describeImportance(importance : Int) : String = {
    import Importance._
    importance match {
      case AUTOMATIC => "AUTOMATIC"
      case COMMIT => "COMMIT"
      case PULL => "PULL"
      case SYNC => "SYNC"
      case MILESTONE => "MILESTONE"
      case _ => importance.toString
    }
  }

  def describeVersion(version : Version) : String = {
    val info = "(" + describeImportance(version.importance) + ", " + version.timestamp + ")"
    "Version " + version.version + " " + info + ": " + version.comment 
  }

  def historyCmd(importance : Int, timespan : TimeSpan, onlyEnabled : Boolean) : Either[String, String] = {
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, version)) =>
        val versions = history(version, importance, timespan, onlyEnabled).toVector
        val size = versions.size
        val output : StringBuilder = new StringBuilder()
        output.append("The current version of branch '" + branch.name + "' is " + branch.currentVersion + ".\n")
        output.append("The working version is " + version.version + ".\n")
        if (branch.master.isDefined) output.append("Its master branch is '" + branch.master.get + "'.\n")
        val e = if (onlyEnabled) "enabled" else "both enabled and disabled"
        val im = if (importance >= 0) "= " + describeImportance(importance) else "≥ " + describeImportance(-importance)
        output.append("Querying " + e + " predecessor versions from " + timespan+" with importance " + im + ".\n")
        if (size == 0)
          output.append("There are no versions which match the query.")
        else if (size == 1)
          output.append("There is one version which matches the query:\n")
        else
          output.append("There are " + size + " versions which match the query:\n")
        for (i <- 1 to size) {
          output.append("\n" + describeVersion(versions(i - 1)))
        }
        output.append("\n\n")
        Left(output.toString)
    }
  }

  def logCmd(importance : Int, _timespan : TimeSpan, onlyEnabled : Boolean) : Either[String, String] = {
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, version)) =>
        val timespan = 
          if (branch.currentVersion != version.version)
            _timespan.limitFuture(version.timestamp)
          else
            _timespan
        val versions = versionary.queryVersions(version.branch, importance, timespan, onlyEnabled)
        val size = versions.size
        val output : StringBuilder = new StringBuilder()
        output.append("The current version of branch '" + branch.name + "' is " + branch.currentVersion + ".\n")
        output.append("The working version is " + version.version + ".\n")
        if (branch.master.isDefined) output.append("Its master branch is '" + branch.master.get + "'.\n")
        val e = if (onlyEnabled) "enabled" else "both enabled and disabled"
        val im = if (importance >= 0) "= " + describeImportance(importance) else "≥ " + describeImportance(-importance)
        output.append("Querying " + e + " versions from " + timespan+" with importance " + im + ".\n")
        if (size == 0)
          output.append("There are no versions which match the query.")
        else if (size == 1)
          output.append("There is one version which matches the query:\n")
        else
          output.append("There are " + size + " versions which match the query:\n")
        for (i <- 1 to size) {
          output.append("\n" + describeVersion(versions(i - 1)))
        }
        output.append("\n\n")
        Left(output.toString)
    }
  }

  def branchCmd(id : Option[String]) : Either[(String, String), String] = {
    val branchname = login + "\\" + (id match {
      case None => "b" + Timestamp.now.toMillis.toString
      case Some(id) => id
    })
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, version)) =>
        val master = Some((branch.name, version.version))
        versionary.createNewBranch(branchname, master, true, login, version.directory) match {
          case Left((newbranch, newversion)) =>
            val branchspec = BranchSpec(Some(newbranch.name), None, currentPath.domain)
            val newpath = Path(Some(branchspec), currentPath.path)
            val status = newpath.toString
            val output = "Created branch '" + branchspec + "'."
            Left((status, output))
          case Right(branch) =>
            Right("Branch '" + branch.name + "' already exists.")
        }
    }
  }

  def branchesCmd(loginOfOwner : Option[String]) : Either[String, String] = {
    val owner = 
      loginOfOwner match {
        case None => login.get
        case Some(loginOfOwner) => loginOfOwner
      }
    val branchnames = versionary.branchesOfLogin(owner)
    val size = branchnames.size
    val output = new StringBuilder()
    if (size == 0)
      output.append("Peer '" + owner + "' owns no branches.\n")
    else {
      if (size == 1) 
        output.append("Peer '" + owner + "' owns one branch:\n")
      else
        output.append("Peer '" + owner + "' owns " + size + " branches:\n")
      for (i <- 1 to size) {
        output.append("" + i + ") " + branchnames(i-1) + "\n")
      }
    }
    Left(output.toString)
  }

  def commitCmd(message : String) : Either[String, String] = {
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, version)) =>
        if (branch.currentVersion != version.version) 
          OUTDATED_VERSION(branch, version)
        else {
          versionary.createNewVersion(branch, login, Importance.COMMIT, message, version.directory,
            version.version, version.masterVersion, Timestamp.now, true) match 
          {
            case Left((newbranch, newversion)) => Left("Committed version " + newversion.version + ".")
            case Right(updatedbranch) => OUTDATED_VERSION(updatedbranch, version)
          }
        }
    }
  }

  def revertCmd(_version : String) : Either[String, String] = {
    val version = _version.toInt
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, currentPathVersion)) =>
        if (branch.currentVersion != currentPathVersion.version)
          OUTDATED_VERSION(branch, currentPathVersion)
        else {
          val v = if (version > 0) version else currentPathVersion.version + version
          loadVersion(branch.name, v) match {
            case None => Right("No such version found.")
            case Some((foundBranch, foundVersion)) =>
              val commitMessage = "Revert to version " + foundVersion.version + "."
              versionary.createNewVersion(foundBranch, login, Importance.AUTOMATIC, commitMessage,
                foundVersion.directory, foundVersion.parentVersion, foundVersion.masterVersion, 
                Timestamp.now, true) match 
              {
                case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, currentPathVersion)
                case Left((newbranch, newversion)) => Left("Reverted to version " + foundVersion.version + ".") 
              }
          }
        }
    }
  }

  def findCommonAncestorVersion(branch : String, version1 : Int, version2 : Int) : Version = {
    def loadv(v : Int) : Version = {
      versionary.lookupVersion(branch, v) match {
        case None => throw new RuntimeException("cannot load version '" + branch + ":" + v + "'")
        case Some(version) => version
      }
    }
    var v1 : Version = loadv(version1)
    var v2 : Version = loadv(version2)
    while (v1.version != v2.version) {
      if (v1.version > v2.version) 
        v1 = loadv(v1.parentVersion)
      else
        v2 = loadv(v2.parentVersion)
    }
    v1
  }

  def pullCmd() : Either[String, String] = {
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, version)) =>
        if (branch.currentVersion != version.version)
          OUTDATED_VERSION(branch, version)
        else if (!branch.master.isDefined) 
          Right("Branch has no master branch.")
        else if (version.directory.countConflicts > 0)
          Right("There are conflicts in the topic branch, please resolve them first.")
        else
          loadVersion(branch.master.get, 0) match {
            case None => Right("Master branch not found.")
            case Some((masterBranch, currentMasterVersion)) =>
              if (currentMasterVersion.directory.countConflicts > 0)
                Right("There are conflicts in the master branch, please resolve them first.")
              else {
                val ancestorVersion = findCommonAncestorVersion(masterBranch.name, 
                  currentMasterVersion.version, version.masterVersion)
                val merge = new Merge(versionary.repository)
                val directory = merge.merge3way(ancestorVersion.directory, currentMasterVersion.directory, 
                  version.directory).asInstanceOf[DirectoryPointer]
                if (directory == version.directory && version.masterVersion == currentMasterVersion.version) 
                  Left("Branch is already up-to-date.")
                else {
                  val commitMessage = "Pulled from master '" + currentMasterVersion + "'."
                  versionary.createNewVersion(branch, login, Importance.PULL, commitMessage,
                    directory, version.version, currentMasterVersion.version, Timestamp.now, true) match 
                  {
                    case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
                    case Left((newbranch, newversion)) =>
                      if (newversion.directory.countConflicts > 0)
                        Left("Pull from master '" + currentMasterVersion + "' resulted in conflicts.")
                      else
                        Left("Successfully pulled from master '" + currentMasterVersion + "'.")
                  }
                }
              }
          }
    }
  }

  def syncCmd() : Either[String, String] = {
    loadVersion(currentPath) match {
      case None => INVALID_PATH
      case Some((branch, version)) =>
        if (branch.currentVersion != version.version)
          OUTDATED_VERSION(branch, version)
        else if (!branch.master.isDefined) 
          Right("Branch has no master branch.")
        else if (version.directory.countConflicts > 0)
          Right("There are conflicts in the topic branch, please resolve them first.")
        else
          loadVersion(branch.master.get, 0) match {
            case None => Right("Master branch not found.")
            case Some((masterBranch, currentMasterVersion)) =>
              if (currentMasterVersion.directory.countConflicts > 0)
                Right("There are conflicts in the master branch, please resolve them first.")
              else {
                val ancestorVersion = findCommonAncestorVersion(masterBranch.name, 
                  currentMasterVersion.version, version.masterVersion)
                val merge = new Merge(versionary.repository)
                val directory = merge.merge3way(ancestorVersion.directory, currentMasterVersion.directory, 
                  version.directory).asInstanceOf[DirectoryPointer]
                if (directory == version.directory && version.masterVersion == currentMasterVersion.version &&
                    directory == currentMasterVersion.directory)
                  Left("Branch is already synchronized.")
                else if (directory.countConflicts > 0) 
                  Right("Synchronizing produces conflicts, please pull and resolve them first.")
                else {
                  val commitMessageMaster = "Synced with topic '" + version + "'."
                  val commitMessageTopic = "Synced with master '" + currentMasterVersion + "'."
                  val now = Timestamp.now
                  versionary.createNewVersion(masterBranch, login, Importance.SYNC, commitMessageMaster,
                    directory, currentMasterVersion.version, currentMasterVersion.masterVersion,
                    now, true) match 
                  {
                    case Right(updatedMasterBranch) => OUTDATED_VERSION(updatedMasterBranch, currentMasterVersion)
                    case Left((newmasterbranch, newmasterversion)) =>
                      versionary.createNewVersion(branch, login, Importance.SYNC, commitMessageTopic,
                        directory, version.version, newmasterversion.version, now, true) match 
                      {
                        case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
                        case Left((newbranch, newversion)) => Left("Successfully synchronized with master branch.")
                      }                    
                  }
                }
              }
          }
    }
  }

  def resolveCmd(path : String, chooseMaster : Boolean) : Either[String, String] = {
    resolvePath(path) match {
      case None => INVALID_PATH
      case Some((branch, version, pointer, path)) =>
        if (branch.currentVersion != version.version) 
          OUTDATED_VERSION(branch, version)
        else pointer match {
          case conflictPointer : ConflictPointer =>
            val chosenPointer = 
              if (chooseMaster) conflictPointer.master else conflictPointer.topic
            val r = versionary.repository
            val directory = r.loadDirectory(version.directory)
            val result =
              chosenPointer match {
                case None =>
                  r.rm(directory, path.path.path.toList) 
                case Some(chosenPointer) =>
                  r.set(directory, path.path.path.toList, chosenPointer)
              } 
            result match {
              case None => Right("Error resolving conflict.")
              case Some((filepath, updatedDirectory)) =>
                val chosen = if (chooseMaster) "MASTER" else "TOPIC"
                val commitMessage = "Resolved conflict " + path + " in favour of " + chosen + "."
                versionary.createNewVersion(branch, login, Importance.AUTOMATIC, commitMessage,
                  updatedDirectory.pointer, version.version, version.masterVersion, Timestamp.now, true) match 
                {
                  case Left((newbranch, newversion)) => Left(commitMessage)
                  case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
                }
            }               
          case _ => Right("Path does not designate a conflict.")
        }
    }
  }

  private def extractTheoryName(path : FilePath) : Option[String] = {
    val ids = path.path
    if (ids.size == 0) return None
    val id = ids(ids.size - 1)
    if (!id.endsWith(".thy") || id.size == 4) return None
    val theoryname = id.substring(0, id.size - 4)
    Some((ids.take(ids.size - 1) :+ theoryname).mkString("\\"))
  }

  private def createNewTheoryContent(theoryname : String) : Content = {
    val content = "theory " + theoryname + "\nextends \\root\n"
    versionary.repository.createContent(ContentTypes.PROOFSCRIPT, content)
  }

  def newTheoryCmd(path : String) : Either[String, String] = {
    resolve(path) match {
      case None => INVALID_PATH
      case Some(path) =>
        extractTheoryName(path.path) match {
          case None => INVALID_PATH
          case Some(theoryname) =>
            loadPath(path) match {
              case Some((branch, version, None)) => 
                if (version.version != branch.currentVersion)
                  OUTDATED_VERSION(branch, version)
                else {
                  val r = versionary.repository
                  val content = createNewTheoryContent(theoryname)
                  r.set(r.loadDirectory(version.directory), path.path.path.toList, content.pointer) match {
                    case None => INVALID_PATH
                    case Some((createdFilePath, createdFile)) =>
                      val comment = "Created theory '" + FilePath(true, createdFilePath.toVector) + "'."
                      versionary.createNewVersion(branch, login, Importance.AUTOMATIC, comment, 
                        createdFile.pointer, version.version, version.masterVersion, Timestamp.now, true) match 
                      {
                        case Left((newBranch, newVersion)) =>
                          val newPath = path.removeVersion
                          val output = "Created theory '" + newPath + "'."
                          Left(output) 
                        case Right(updatedBranch) => OUTDATED_VERSION(updatedBranch, version)
                      }
                  }
                }
              case Some(_) =>
                Right("The theory already exists.")
              case None => INVALID_PATH
            }            
        }
    }
  }

  def readTheory(path : String) : Option[(Int, String, String)] = {
    resolvePath(path) match {
      case None => None
      case Some((branch, version, valuepointer, path)) =>
        valuepointer match {
          case pointer: ContentPointer if pointer.contentTypeId == ContentTypes.PROOFSCRIPT =>
            val content = versionary.repository.loadContent(pointer).get.asInstanceOf[String]
            Some((branch.currentVersion, path.toString, content))
          case _ => None
        }
    }
  }

}