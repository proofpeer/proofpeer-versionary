package proofpeer.versionary 

class Timestamp private (millis : Long) {
  def toMillis : Long = millis
  override def toString : String = {
    TimestampNative.format(millis)
  }
}

object Timestamp {

  def now = Timestamp(System.currentTimeMillis)

  def apply(millis : Long) : Timestamp = new Timestamp(millis)

}

case class TimeSpan(since : Option[Long], until : Option[Long]) {
  /** Returns true if t is in the present or future of this timespan. */
  def inPresentOrFuture(t : Timestamp) : Boolean = {
    since match {
      case None => true
      case Some(s) => s <= t.toMillis
    }
  }

  /** Returns true if t is in the present or past of this timespan. */
  def inPresentOrPast(t : Timestamp) : Boolean = {
    until match {
      case None => true
      case Some(s) => s >= t.toMillis
    }
  }

  def inPresent(t : Timestamp) : Boolean = 
    inPresentOrFuture(t) && inPresentOrPast(t)

  def limitFuture(t : Timestamp) : TimeSpan = {
    until match {
      case Some(s) if (s <= t.toMillis) => this
      case _ => TimeSpan(since, Some(t.toMillis))
    }
  }

  override def toString : String = {
    val u = if (since.isDefined) Timestamp(since.get).toString else "*"
    val v = if (until.isDefined) Timestamp(until.get).toString else "*"
    u + " - " + v
  }

}
