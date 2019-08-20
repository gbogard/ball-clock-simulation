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
    val leastRecentlyUsedBall = currentState.bottomTrack.balls.lastOption

    val takeLeastRecentlyUsedBallOut =
      bottomTrack.modify(track => {
        track.copy(balls = track.balls.dropRight(1))
      })

    val addBallToOneMinuteTrack =
      (oneMinuteTrack composeLens ballsLens).modify(_ ++ leastRecentlyUsedBall)

    val handleFifthBallInOneMinuteTrack = (clock: Clock) => {
      if (clock.oneMinute.balls.length > clock.oneMinute.maxCapacity) {
        val (newOneMinuteTrack, newBottomTrack, newFiveMinutesTrack) =
          handleTrackOverflow(clock.oneMinute, clock.bottomTrack, clock.fiveMinutes)
        clock.copy(
          oneMinute = newOneMinuteTrack,
          fiveMinutes = newFiveMinutesTrack,
          bottomTrack = newBottomTrack
        )
      } else clock
    }

    val handleTwelfthBallInFiveMinuteTrack = (clock: Clock) => {
      if (clock.fiveMinutes.balls.length > clock.fiveMinutes.maxCapacity) {
        val (newFiveMinutesTrack, newBottomTrack, newOneHourTrack) =
          handleTrackOverflow(clock.fiveMinutes, clock.bottomTrack, clock.oneHour)
        clock.copy(
          fiveMinutes = newFiveMinutesTrack,
          oneHour = newOneHourTrack,
          bottomTrack = newBottomTrack
        )
      } else clock
    }

    val handleTwelfthBallInOneHourTrack = (clock: Clock) => {
      if (clock.oneHour.balls.length > clock.oneHour.maxCapacity) {
        val (newOneHourTrack, _, newBottomTrack) =
          handleTrackOverflow(clock.oneHour, clock.bottomTrack, clock.bottomTrack)
        clock.copy(
          oneHour = newOneHourTrack,
          bottomTrack = newBottomTrack
        )
      } else clock
    }

    (
      takeLeastRecentlyUsedBallOut
        andThen addBallToOneMinuteTrack
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
      else if (tickCounter < 100) loop(tick(state), tickCounter + 1)
      else throw new RuntimeException("Possible infinite loop detected")
    }

    loop(initialState, 0)
  }

  private def sendLastBallToTrack(
      inputTrack: Track,
      targetTrack: Track
  ): (Track, Track) = {
    val lastBall = inputTrack.balls.lastOption
    (
      inputTrack.copy(balls = Nil),
      ballsLens.modify(_ ++ lastBall)(targetTrack)
    )
  }

  private def sendNLastBallsToTrack(
      inputTrack: Track,
      targetTrack: Track,
      n: Int
  ): (Track, Track) = {
    (
      ballsLens.modify(_.drop(n))(inputTrack),
      ballsLens.modify(inputTrack.balls.take(n).reverse ++ _)(targetTrack)
    )
  }

  private def handleTrackOverflow(
      overflowedTrack: Track,
      bottomTrack: Track,
      destinationForLastBall: Track
  ): (Track, Track, Track) = {
    val (trackWithoutOverflow, newBottomTrack) = sendNLastBallsToTrack(
      overflowedTrack,
      bottomTrack,
      overflowedTrack.balls.length - 1
    )
    val (emptyTrack, newDestinationTrack) =
      sendLastBallToTrack(trackWithoutOverflow, destinationForLastBall)
    (emptyTrack, newBottomTrack, newDestinationTrack)
  }

  private val oneMinuteTrack: Lens[Clock, Track] = GenLens[Clock](_.oneMinute)
  private val fiveMinutesTrack: Lens[Clock, Track] =
    GenLens[Clock](_.fiveMinutes)
  private val oneHourTrack: Lens[Clock, Track] = GenLens[Clock](_.oneHour)
  private val bottomTrack: Lens[Clock, Track] = GenLens[Clock](_.bottomTrack)
  private val ballsLens: Lens[Track, List[Ball]] = GenLens[Track](_.balls)
}
