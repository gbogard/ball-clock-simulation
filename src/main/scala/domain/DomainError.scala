package domain

sealed trait DomainError

object DomainError {
  case object BallsNumberOutOfRange extends DomainError
}