package domain

sealed trait UserInput

object UserInput {
  case class RunForDuration(balls: Int, minutes: Int) extends UserInput
  case class RunUntilCompleteCycle(balls: Int) extends UserInput
}
