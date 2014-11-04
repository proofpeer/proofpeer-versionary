package proofpeer.versionary

private object TimestampNative {

  import java.util._
  import java.text._

  def format(millis : Long) : String = {
    val gmtFormat = new SimpleDateFormat()
    val gmtTime = TimeZone.getTimeZone("GMT")
    gmtFormat.setTimeZone(gmtTime)
    gmtFormat.format(new Date(millis))
  }

}
