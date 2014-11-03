package proofpeer.versionary 

class Timestamp private (millis : Long) {
  def toMillis : Long = millis
}

object Timestamp {

  def now = Timestamp(System.currentTimeMillis)

  def apply(millis : Long) : Timestamp = new Timestamp(millis)

}

case class TimeSpan(since : Option[Long], until : Option[Long])
