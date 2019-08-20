package application

import scala.concurrent.duration._

object Utils {
  def convertMinutesToDays(minutes: Int): Long = minutes.minutes.toDays
}