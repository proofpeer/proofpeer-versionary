package proofpeer.versionary

import proofpeer.general.CaseInsensitiveMap

class Merge(repository : Repository) {

  def merge3way(original : Directory, master : Directory, topic : Directory) : Directory = 
  {
    var mergedEntries : List[(String, ValuePointer)] = List()
    var masterEntries = master.entries.toList
    var topicEntries = topic.entries.toList
    var originals : CaseInsensitiveMap[(String, ValuePointer)] = CaseInsensitiveMap()
    for (e <- original.entries) originals = originals.put(e._1, e)
    def mergeStandaloneEntry(entry : (String, ValuePointer), isMasterEntry : Boolean) {
      originals.get(entry._1) match {
        case None =>
          mergedEntries = entry :: mergedEntries
        case Some((originalName, originalPointer)) =>
          if (originalPointer != entry._2) {
            val conflict = 
              if (isMasterEntry)
                repository.createConflict(Some(entry._2), None)
              else
                repository.createConflict(None, Some(entry._2))
            mergedEntries = (entry._1, conflict.pointer) :: mergedEntries
          } else {
            // do nothing, this will delete the entry
          }
      }            
    }
    while (!masterEntries.isEmpty && !topicEntries.isEmpty) {
      val masterEntry = masterEntries.head
      val topicEntry = topicEntries.head
      val c = masterEntry._1.toLowerCase compare topicEntry._1.toLowerCase
      if (c == 0) {
        originals.get(masterEntry._1) match {
          case None =>
            mergedEntries = (masterEntry._1, merge2way(masterEntry._2, topicEntry._2)) :: mergedEntries
          case Some((originalName, originalPointer)) =>
            val name = if (originalName == masterEntry._1) topicEntry._1 else masterEntry._1
            mergedEntries = (name, merge3way(originalPointer, masterEntry._2, topicEntry._2)) :: mergedEntries
        }
        masterEntries = masterEntries.tail
        topicEntries = topicEntries.tail
      } else if (c < 0) {
        mergeStandaloneEntry(masterEntry, true)
        masterEntries = masterEntries.tail
      } else {
        mergeStandaloneEntry(topicEntry, false)
        topicEntries = topicEntries.tail
      }
    }
    for (masterEntry <- masterEntries) mergeStandaloneEntry(masterEntry, true)
    for (topicEntry <- topicEntries) mergeStandaloneEntry(topicEntry, true)
    repository.createDirectory(mergedEntries.reverse.toVector)
  }

  def merge2way(master : Directory, topic : Directory) : Directory = {
    var mergedEntries : List[(String, ValuePointer)] = List()
    var masterEntries = master.entries.toList
    var topicEntries = topic.entries.toList
    def mergeStandaloneEntry(entry : (String, ValuePointer), isMasterEntry : Boolean) {
      if (isMasterEntry)
        repository.createConflict(Some(entry._2), None)
      else
        repository.createConflict(None, Some(entry._2))
    }
    while (!masterEntries.isEmpty && !topicEntries.isEmpty) {
      val masterEntry = masterEntries.head
      val topicEntry = topicEntries.head
      val c = masterEntry._1.toLowerCase compare topicEntry._1.toLowerCase
      if (c == 0) {
        mergedEntries = (masterEntry._1, merge2way(masterEntry._2, topicEntry._2)) :: mergedEntries
        masterEntries = masterEntries.tail
        topicEntries = topicEntries.tail
      } else if (c < 0) {
        mergeStandaloneEntry(masterEntry, true)
        masterEntries = masterEntries.tail
      } else {
        mergeStandaloneEntry(topicEntry, false)
        topicEntries = topicEntries.tail
      }
    }
    for (masterEntry <- masterEntries) mergeStandaloneEntry(masterEntry, true)
    for (topicEntry <- topicEntries) mergeStandaloneEntry(topicEntry, true)
    repository.createDirectory(mergedEntries.reverse.toVector)    
  }

  def merge3way(originalPointer : ValuePointer, masterPointer : ValuePointer, topicPointer : ValuePointer) : ValuePointer = {
    if (masterPointer == topicPointer) return masterPointer
    if (originalPointer == masterPointer) return topicPointer
    if (originalPointer == topicPointer) return masterPointer
    (originalPointer, masterPointer, topicPointer) match {
      case (originalPointer : DirectoryPointer, masterPointer : DirectoryPointer, topicPointer : DirectoryPointer) =>
        val original = repository.loadDirectory(originalPointer)
        val master = repository.loadDirectory(masterPointer)
        val topic = repository.loadDirectory(topicPointer)
        merge3way(original, master, topic).pointer
      case (_, masterPointer : DirectoryPointer, topicPointer : DirectoryPointer) =>
        val master = repository.loadDirectory(masterPointer)
        val topic = repository.loadDirectory(topicPointer)
        merge2way(master, topic).pointer
      case (originalPointer : ContentPointer, masterPointer : ContentPointer, topicPointer : ContentPointer) 
        if (originalPointer.contentTypeId == masterPointer.contentTypeId && 
          masterPointer.contentTypeId == topicPointer.contentTypeId)
      =>
        val original = repository.loadContent(originalPointer)
        val master = repository.loadContent(masterPointer)
        val topic = repository.loadContent(topicPointer)
        val contentType = ContentTypes.contentTypeOf(originalPointer.contentTypeId)
        contentType.merge3way(original.get, master.get, topic.get) match {
          case None => repository.createConflict(Some(masterPointer), Some(topicPointer)).pointer
          case Some(mergeResult) =>
            repository.createContent(contentType.contentTypeId, mergeResult).pointer
        }
      case (_, masterPointer : ContentPointer, topicPointer : ContentPointer) 
        if (masterPointer.contentTypeId == topicPointer.contentTypeId)
      =>
        val master = repository.loadContent(masterPointer)
        val topic = repository.loadContent(topicPointer)
        val contentType = ContentTypes.contentTypeOf(masterPointer.contentTypeId)
        contentType.merge2way(master.get, topic.get) match {
          case None => repository.createConflict(Some(masterPointer), Some(topicPointer)).pointer
          case Some(mergeResult) =>
            repository.createContent(contentType.contentTypeId, mergeResult).pointer
        }
      case _ => repository.createConflict(Some(masterPointer), Some(topicPointer)).pointer
    }
  }

  def merge2way(masterPointer : ValuePointer, topicPointer : ValuePointer) : ValuePointer = {
    if (masterPointer == topicPointer) return masterPointer
    (masterPointer, topicPointer) match {
      case (masterPointer : DirectoryPointer, topicPointer : DirectoryPointer) =>
        val master = repository.loadDirectory(masterPointer)
        val topic = repository.loadDirectory(topicPointer)
        merge2way(master, topic).pointer
      case (masterPointer : ContentPointer, topicPointer : ContentPointer) 
        if (masterPointer.contentTypeId == topicPointer.contentTypeId)
      =>
        val master = repository.loadContent(masterPointer)
        val topic = repository.loadContent(topicPointer)
        val contentType = ContentTypes.contentTypeOf(masterPointer.contentTypeId)
        contentType.merge2way(master.get, topic.get) match {
          case None => repository.createConflict(Some(masterPointer), Some(topicPointer)).pointer
          case Some(mergeResult) =>
            repository.createContent(contentType.contentTypeId, mergeResult).pointer
        }
      case _ => repository.createConflict(Some(masterPointer), Some(topicPointer)).pointer
    }
  }

}