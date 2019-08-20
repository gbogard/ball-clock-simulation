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
      tiltTrackOptional(clock.oneMinute) match {
        case Some((fiveMinuteBall, rest)) =>
          ((oneMinuteTrack composeLens ballsLens).set(Nil)
            andThen (bottomTrack composeLens ballsLens).modify(
              rest.reverse ++ _
            )
            andThen (fiveMinutesTrack composeLens ballsLens).modify(
              _ :+ fiveMinuteBall
            ))(clock)
        case None => clock
      }
    }

    val handleTwelfthBallInFiveMinuteTrack = (clock: Clock) => {
      tiltTrackOptional(clock.fiveMinutes) match {
        case Some((oneHourBall, rest)) =>
          ((fiveMinutesTrack composeLens ballsLens).set(Nil)
            andThen (bottomTrack composeLens ballsLens).modify(
              rest.reverse ++ _
            )
            andThen (oneHourTrack composeLens ballsLens).modify(
              _ :+ oneHourBall
            ))(clock)
        case None => clock
      }
    }

    val handleTwelfthBallInOneHourTrack = (clock: Clock) => {
      tiltTrackOptional(clock.oneHour) match {
        case Some((lastBall, rest)) =>
          ((oneHourTrack composeLens ballsLens).set(Nil)
            andThen (bottomTrack composeLens ballsLens).modify(
              rest.reverse ++ _
            )
            andThen (bottomTrack composeLens ballsLens).modify(
              _ :+ lastBall
            ))(clock)
        case None => clock
      }
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
      else if (tickCounter < 1000000) loop(tick(state), tickCounter + 1)
      else throw new RuntimeException("Possible infinite loop detected")
    }

    loop(initialState, 0)
  }

  /**
    * Returns the ball that caused the tilt along with the balls that
    * are rolling down from the track to the bottom track
    */
  private def tiltTrackOptional(track: Track): Option[(Ball, List[Ball])] =
    if (track.balls.length > track.maxCapacity)
      track.balls.lastOption.map((_, track.balls.dropRight(1)))
    else None

  private val oneMinuteTrack: Lens[Clock, Track] = GenLens[Clock](_.oneMinute)
  private val fiveMinutesTrack: Lens[Clock, Track] =
    GenLens[Clock](_.fiveMinutes)
  private val oneHourTrack: Lens[Clock, Track] = GenLens[Clock](_.oneHour)
  private val bottomTrack: Lens[Clock, Track] = GenLens[Clock](_.bottomTrack)
  private val ballsLens: Lens[Track, List[Ball]] = GenLens[Track](_.balls)
}
