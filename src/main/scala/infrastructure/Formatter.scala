package infrastructure

import io.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import domain.Output._
import domain.DomainError._
import domain._

object Formatter {
  implicit private val trackEncoder: Encoder[Track] =
    Encoder.instance[Track](_.balls.map(_.number).asJson)

  implicit private val clockEncoder: Encoder[Clock] = Encoder.instance[Clock](
    clock =>
      Json.obj(
        "Min" -> clock.oneMinute.asJson,
        "FiveMin" -> clock.fiveMinutes.asJson,
        "Hour" -> clock.oneHour.asJson,
        "Main" -> clock.bottomTrack.asJson
      )
  )

  def formatOutput(output: Output): String = output match {
    case ClockState(state)         => state.asJson.noSpaces
    case NumberOfDays(balls, days) => s"$balls balls cycle after $days days."
  }

  def formatDomainError(error: DomainError): String = error match {
    case BallsNumberOutOfRange =>
      "- Number of balls is out of range. There should be between 27 and 127 balls."
  }

}
