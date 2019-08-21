package application

import domain._
import monocle.Lens
import monocle.macros.GenLens
import domain.UserInput.RunForDuration
import domain.UserInput.RunUntilCompleteCycle

object StateMachine {

  def handUserInput(userInput: UserInput): Either[DomainError, Output] =
    userInput match {
      case RunForDuration(balls, minutes) =>
        getInitialState(balls)
          .map(state => runForDuration(minutes, state))
          .map(Output.ClockState)
      case RunUntilCompleteCycle(balls) =>
        getInitialState(balls)
          .map(state => runUntilInitialOrdering(state))
          .map(Utils.convertMinutesToDays)
          .map(Output.NumberOfDays(balls, _))
    }

  def getInitialState(numberOfBalls: Int): Either[DomainError, Clock] =
    if (27 <= numberOfBalls && numberOfBalls <= 127) {
      val balls = (1 to numberOfBalls).map(i => Ball(i)).toList
      Right(
        Clock(
          oneMinute = Track(maxCapacity = 4),
          fiveMinutes = Track(maxCapacity = 11),
          oneHour = Track(maxCapacity = 11),
          bottomTrack = Track(balls, numberOfBalls)
        )
      )
    } else {
      Left(DomainError.BallsNumberOutOfRange)
    }

  def tick(currentState: Clock): Clock = {
    val putBallInOneMinuteTrack = sendLeastRecentlyUsedBallToTrack(bottomTrack, oneMinuteTrack) _

    val handleFifthBallInOneMinuteTrack = (clock: Clock) => {
      if (clock.oneMinute.balls.length > clock.oneMinute.maxCapacity) {
        handleTrackOverflow(oneMinuteTrack, bottomTrack, fiveMinutesTrack)(clock)
      } else clock
    }

    val handleTwelfthBallInFiveMinuteTrack = (clock: Clock) => {
      if (clock.fiveMinutes.balls.length > clock.fiveMinutes.maxCapacity) {
        handleTrackOverflow(fiveMinutesTrack, bottomTrack, oneHourTrack)(clock)
      } else clock
    }

    val handleTwelfthBallInOneHourTrack = (clock: Clock) => {
      if (clock.oneHour.balls.length > clock.oneHour.maxCapacity) {
        handleTrackOverflow(oneHourTrack, bottomTrack, bottomTrack)(clock)
      } else clock
    }

    (
      putBallInOneMinuteTrack
        andThen handleFifthBallInOneMinuteTrack
        andThen handleTwelfthBallInFiveMinuteTrack
        andThen handleTwelfthBallInOneHourTrack
    )(currentState)
  }

  /**
    * Runs the clock for a specific duration and returns
    * its last state
    */
  def runForDuration(
      duration: Int,
      initialState: Clock
  ): Clock =
    (1 to duration).foldLeft(initialState)({
      case (state, _) => tick(state)
    })

  /**
    * Returns the number of minutes until the clock gets back to
    * its initial ordering
    */
  def runUntilInitialOrdering(initialState: Clock): Int = {
    def loop(state: Clock, tickCounter: Int): Int = {
      if (state.bottomTrack == initialState.bottomTrack && tickCounter != 0)
        tickCounter
      else if (tickCounter < 10000000) loop(tick(state), tickCounter + 1)
      else throw new RuntimeException("Possible infinite loop detected")
    }

    loop(initialState, 0)
  }

  private def sendLeastRecentlyUsedBallToTrack(
      inputTrack: Lens[Clock, Track],
      targetTrack: Lens[Clock, Track]
  )(clock: Clock): Clock = {
    val leastRecentlyUsedBall = inputTrack.get(clock).balls.headOption

    val takeLeastRecentlyUsedBallOut =
      (inputTrack composeLens ballsLens).modify(_.drop(1))

    val addBallToTargetTrack =
      (targetTrack composeLens ballsLens).modify(_ ++ leastRecentlyUsedBall)
    (takeLeastRecentlyUsedBallOut andThen addBallToTargetTrack)(clock)
  }

  private def sendMostRecentlyUsedBallToTrack(
      inputTrack: Lens[Clock, Track],
      targetTrack: Lens[Clock, Track]
  )(clock: Clock): Clock = {
    val mostRecentlyUsedBall = inputTrack.get(clock).balls.lastOption
    val takeMostRecentlyUsedBallOut =
      (inputTrack composeLens ballsLens).modify(_.dropRight(1))
    val addBallToTargetTrack =
      (targetTrack composeLens ballsLens).modify(_ ++ mostRecentlyUsedBall)
    (takeMostRecentlyUsedBallOut andThen addBallToTargetTrack)(clock)
  }

  private def handleTrackOverflow(
      overFlowedTrack: Lens[Clock, Track],
      bottomTrack: Lens[Clock, Track],
      destinationForFrontBall: Lens[Clock, Track],
      lastBallFirst: Boolean = true
  ): Clock => Clock = {
    ((clock: Clock) => {
      val overFlowedTrackBalls = overFlowedTrack.get(clock).balls.dropRight(1)
      ((overFlowedTrack composeLens ballsLens).modify(_.takeRight(1))
        andThen (
          (bottomTrack composeLens ballsLens).modify(_ ++ overFlowedTrackBalls.reverse)
        ))(clock)
    }) andThen (sendMostRecentlyUsedBallToTrack(
      overFlowedTrack,
      destinationForFrontBall
    ) _) 
  }

  private val oneMinuteTrack: Lens[Clock, Track] = GenLens[Clock](_.oneMinute)
  private val fiveMinutesTrack: Lens[Clock, Track] =
    GenLens[Clock](_.fiveMinutes)
  private val oneHourTrack: Lens[Clock, Track] = GenLens[Clock](_.oneHour)
  private val bottomTrack: Lens[Clock, Track] = GenLens[Clock](_.bottomTrack)
  private val ballsLens: Lens[Track, List[Ball]] = GenLens[Track](_.balls)
}
