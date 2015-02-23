package proofpeer.versionary

object StorageImpl {

  def get() : Storage = new GAEStorage("VersionaryStorage")

}