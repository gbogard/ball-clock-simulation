package domain

sealed trait Output

object Output {
  case class NumberOfDays(balls: Int, days: Int) extends Output
  case class ClockState(clock: Clock) extends Output
}