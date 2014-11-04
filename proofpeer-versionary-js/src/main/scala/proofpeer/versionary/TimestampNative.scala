package proofpeer.versionary

import scala.scalajs.js

private object TimestampNative {

  def format(millis : Long) : String = {
    val date = new js.Date(millis)
    date.toUTCString()
  }

}
