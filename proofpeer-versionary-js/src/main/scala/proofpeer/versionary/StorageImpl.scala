package proofpeer.versionary

object StorageImpl {

  def get() : Storage = new HTML5Storage("VersionaryStorage_")

}