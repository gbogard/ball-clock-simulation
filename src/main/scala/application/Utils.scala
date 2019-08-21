package application

import scala.concurrent.duration._

object Utils {
  def convertMinutesToDays(minutes: Int): Int = minutes.minutes.toDays.toInt
}