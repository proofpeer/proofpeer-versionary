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

class VersionaryConsole(versionary : Versionary, login : String, currentPath : Path, domain : String) {

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

  def mkdirCmd(path : String) : Either[(String, String), String] = {
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
                  versionary.createNewVersion(branch, Some(login), Importance.AUTOMATIC, comment, 
                    createdDirectory.pointer, version.version, version.masterVersion, Timestamp.now, true) match 
                  {
                    case Left((newBranch, newVersion)) =>
                      val newPath = path.removeVersion
                      val output = "Created directory '" + (path.removeVersion) + "'."
                      Left((newPath.toString, output)) 
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

}