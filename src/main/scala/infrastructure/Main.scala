package infrastructure

import domain.UserInput
import domain.UserInput._
import infrastructure.Extractors._
import application.StateMachine

object Main {
  def main(args: Array[String]): Unit = {
    parseInput(args.toList) match {
      case Some(input) => StateMachine.handUserInput(input) match {
        case Right(output) => println(Formatter.formatOutput(output))
        case Left(error) => 
          println(Formatter.formatDomainError(error))
          sys.exit(1)
      }
      case None => println(helpMessage)
    }
  }

  private def parseInput(args: List[String]): Option[UserInput] = args match {
    case Int(nbOfBalls) :: Nil => Some(RunUntilCompleteCycle(nbOfBalls)) 
    case Int(nbOfBalls) :: Int(minutes) :: Nil => Some(RunForDuration(nbOfBalls, minutes))
    case _ => None
  }

  private val helpMessage: String = """
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
  |     {"Min":[],"FiveMin":[13,8,17,6,28],"Hour":[23,5,20,24,29],"Main":[3,30,4,12,10,21,1,22,7,25,11,27,9,16,19,14,26,2,18,15]}
  """.stripMargin.trim()
}