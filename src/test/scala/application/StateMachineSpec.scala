package application

import org.scalatest._
import domain._

class StateMachineSpec extends FunSpec with Matchers with EitherValues {
  describe("State Machine") {
    describe("Initial state") {
      it(
        "should create 3 empty tracks and fill the bottom track with 27 numbered balls when the number of balls is 27"
      ) {
        StateMachine.getInitialState(27).right.value shouldBe
          Clock(
            oneMinute = Track(maxCapacity = 4),
            fiveMinutes = Track(maxCapacity = 11),
            oneHour = Track(List(), maxCapacity = 11),
            bottomTrack = Track(
              List(
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23,
                24, 25, 26, 27
              ).map(Ball),
              27
            )
          )
      }

      it("Should fail when the number of balls is below 27") {
        StateMachine
          .getInitialState(18)
          .left
          .value shouldBe DomainError.BallsNumberOutOfRange
      }

      it("Should fail when the number of balls is above 127") {
        StateMachine
          .getInitialState(128)
          .left
          .value shouldBe DomainError.BallsNumberOutOfRange
      }
    }

    describe("Ticking") {

      it(
        "Should take the last ball from the bottom track and put it in the oneMinute track"
      ) {
        val currentState = Clock(
          oneMinute = Track(List(1, 2).map(Ball), 4),
          fiveMinutes = Track(Nil, 11),
          oneHour = Track(Nil, 11),
          bottomTrack = Track(List(3, 4, 5).map(Ball), 27)
        )

        val expectedState = currentState.copy(
          oneMinute = Track(List(1, 2, 5).map(Ball), 4),
          bottomTrack = Track(List(3, 4).map(Ball), 27)
        )

        StateMachine.tick(currentState) shouldBe expectedState
      }

      it(
        "Should put the fifth ball in the five minutes track when the one minute track is full"
      ) {
        val currentState = Clock(
          oneMinute = Track(List(1, 2, 3, 4).map(Ball), 4),
          fiveMinutes = Track(List(Ball(5)), 11),
          oneHour = Track(Nil, 11),
          bottomTrack = Track(List(6, 7, 8).map(Ball), 27)
        )

        val expectedState = currentState.copy(
          oneMinute = Track(Nil, 4),
          fiveMinutes = Track(List(5, 8).map(Ball), 11),
          bottomTrack = Track(List(4, 3, 2, 1, 6, 7).map(Ball), 27)
        )

        StateMachine.tick(currentState) shouldBe expectedState
      }

      it(
        "Should put the twelfth ball in the one hour track when the five minutes track is full"
      ) {
        val currentState = Clock(
          oneMinute = Track(List(1, 2, 3, 4).map(Ball), 4),
          fiveMinutes = Track(List(5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15).map(Ball), 11),
          oneHour = Track(Nil, 11),
          bottomTrack = Track(List(16, 17, 18, 19, 20).map(Ball), 27)
        )

        val expectedState = currentState.copy(
          oneMinute = Track(Nil, 4),
          fiveMinutes = Track(Nil, 11),
          oneHour = Track(List(Ball(20)), 11),
          bottomTrack = Track(
            List(15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 16, 17, 18, 19).map(Ball),
            27
          )
        )

        StateMachine.tick(currentState) shouldBe expectedState
      }
    }

    describe("Run for duration") {
      it("Should return a valid clock state after 1 minute with 27 balls") {
        StateMachine
          .getInitialState(27)
          .map(StateMachine.runForDuration(1, _))
          .right
          .map { result =>
            result.oneHour.balls.length shouldBe 0
            result.fiveMinutes.balls.length shouldBe 0
            result.oneMinute.balls.length shouldBe 1
            result.bottomTrack.balls.length shouldBe 26
          }
      }

      it("Should return a valid clock state after 5 minutes with 27 balls") {
        StateMachine
          .getInitialState(27)
          .map(StateMachine.runForDuration(5, _))
          .right
          .map { result =>
            result.oneHour.balls.length shouldBe 0
            result.fiveMinutes.balls.length shouldBe 1
            result.oneMinute.balls.length shouldBe 0
            result.bottomTrack.balls.length shouldBe 26
          }
      }

      it("Should return a valid clock state after 325 minutes with 30 balls") {
        StateMachine
          .getInitialState(30)
          .map(StateMachine.runForDuration(325, _))
          .right
          .map { result =>
            result.oneHour.balls.length shouldBe 5
            result.fiveMinutes.balls.length shouldBe 5
            result.oneMinute.balls.length shouldBe 0
            result.bottomTrack.balls.length shouldBe 20
          }
      }

      it("Should return a valid clock state after 420 minutes with 30 balls") {
        StateMachine
          .getInitialState(30)
          .map(StateMachine.runForDuration(420, _))
          .right
          .map { result =>
            result.oneHour.balls.length shouldBe 7
            result.fiveMinutes.balls.length shouldBe 0
            result.oneMinute.balls.length shouldBe 0
            result.bottomTrack.balls.length shouldBe 23
          }
      }

      it("Should return a valid clock state after 436 minutes with 40 balls") {
        StateMachine
          .getInitialState(40)
          .map(StateMachine.runForDuration(436, _))
          .right
          .map { result =>
            result.oneHour.balls.length shouldBe 7
            result.fiveMinutes.balls.length shouldBe 3
            result.oneMinute.balls.length shouldBe 1
            result.bottomTrack.balls.length shouldBe 29
          }
      }
    }

    describe("Run until initial order") {
      it("Should return 15 days when the clock has 30 balls") {
        StateMachine
          .getInitialState(30)
          .map(StateMachine.runUntilInitialOrdering)
          .map(Utils.convertMinutesToDays)
          .right
          .value shouldBe 15
      }

      it("Should return 278 days when the clock has 30 balls") {
        StateMachine
          .getInitialState(45)
          .map(StateMachine.runUntilInitialOrdering)
          .map(Utils.convertMinutesToDays)
          .right
          .value shouldBe 278
      }
    }
  }

}
