package infrastructure

import cats.effect._
import domain._
import domain.UserInput._
import infrastructure.Extractors._
import application.StateMachine

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    IO.pure(parseInput(args)) flatMap {
      case Some(input) => IO.pure(StateMachine.handUserInput(input)).flatMap(printResult)
      case None => IO {
        println(helpMessage)
        ExitCode.Error
      }
    }
  }

  private def printResult(result: Either[DomainError, Output]): IO[ExitCode] = result match {
    case Right(output) =>
      IO {
        println(Formatter.formatOutput(output))
        ExitCode.Success
      }
    case Left(error) =>
      IO {
        println(Formatter.formatDomainError(error))
        ExitCode.Error
      }
  }

  private def parseInput(args: List[String]): Option[UserInput] = args match {
    case Int(nbOfBalls) :: Nil => Some(RunUntilCompleteCycle(nbOfBalls))
    case Int(nbOfBalls) :: Int(minutes) :: Nil => Some(RunForDuration(nbOfBalls, minutes))
    case _ => None
  }


  private val helpMessage: String =
    """
      |Ball clock simulation. Help:
      |
      | - <number of balls>
      |    If only the number of balls is supplied, the program will return the number of days until the
      |    clock returns to its original ordering.
      |
      |      Sample Input
      |      30
      |      Output for the sample input
      |      30 balls cycle after 15 days.
      |
      | - <number of balls> <minutes>
      |   If the number of minutes is supplied, the program will return the state of the clock after running for the
      |   supplied amount of minutes, in JSON format.
      |
      |     Sample Input
      |     30 325
      |     Output for the sample input
      |     {"Min":[],"FiveMin":[22,13,25,3,7],"Hour":[6,12,17,4,15],"Main":[11,5,26,18,2,30,19,8,24,10,29,20,16,21,28,1,23,14,27,9]}
  """.stripMargin.trim()
}