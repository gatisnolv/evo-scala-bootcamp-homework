package com.gatis.homework.effects

import org.scalatest.Assertion
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import com.gatis.homework.effects.EffectsHomework1._
import scala.util.Success
import scala.util.Failure
import scala.util.Try

class EffectsHomework1Spec extends AnyFreeSpec with Matchers {
  "map works for IO" in {
    IO("something").map(_.length).unsafeRunSync() shouldBe 9
  }

  "flatMap works for IO" in {
    IO("something").flatMap(x => IO(x.length)).unsafeRunSync() shouldBe 9
  }

  "*> works for IO discarding first result" in {
    (IO("something") *> IO("different")).unsafeRunSync() shouldBe "different"
  }

  "as works for IO discarding first result" in {
    (IO("something") as "different").unsafeRunSync() shouldBe "different"
  }

  "void works for IO discarding first result and returning unit" in {
    IO("something").void.unsafeRunSync() shouldBe ()
  }

  "attempt works for IO returning throwable in left" in {
    val exception = new Exception
    IO(throw exception).attempt.unsafeRunSync() shouldBe Left(exception)
  }

  "attempt works for IO returning result in right" in {
    IO("something").attempt.unsafeRunSync() shouldBe Right("something")
  }

  "option works for IO returning none when an exception is thrown" in {
    val exception = new Exception
    IO(throw exception).option.unsafeRunSync() shouldBe None
  }

  "option works for IO returning result wrapped in Some" in {
    IO("something").option.unsafeRunSync() shouldBe Some("something")
  }

  "handleErrorWith works for IO handling error when an exception is thrown" in {
    val exception = new Exception("message")
    IO(throw exception).handleErrorWith(e => IO(s"nice ${e.getMessage()}")).unsafeRunSync() shouldBe "nice message"
  }

  "handleErrorWith works for IO returning result when no exception thrown" in {
    IO("something").handleErrorWith(e => IO(s"nice ${e.getMessage()}")).unsafeRunSync() shouldBe "something"
  }

  "redeem works for IO handling error when an exception is thrown" in {
    val exception = new Exception("message")
    (IO(throw exception): IO[String]).redeem(e => s"nice ${e.getMessage()}", _.length).unsafeRunSync() shouldBe "nice message"
  }

  "redeem works for IO returning result when no exception thrown" in {
    IO("something").redeem(e => s"nice ${e.getMessage()}", _.length).unsafeRunSync() shouldBe 9
  }

  "redeemWith works for IO handling error when an exception is thrown" in {
    val exception = new Exception("message")
    (IO(throw exception): IO[String]).redeemWith(e => IO(s"nice ${e.getMessage()}"), x => IO(s"$x different")).unsafeRunSync() shouldBe "nice message"
  }

  "redeemWith works for IO returning result when no exception thrown" in {
    IO("something").redeemWith(e => IO(s"nice ${e.getMessage()}"), x => IO(s"$x different")).unsafeRunSync() shouldBe "something different"
  }

  "unsafeRunSync evaluates the IO" in {
    IO("something").unsafeRunSync() shouldBe "something"
  }

  "unsafeToFuture evaluates the IO to a future" in {
    IO("something").unsafeToFuture().onComplete {
      case Success(value) => value shouldBe "something"
      case Failure(e)     => fail()
    }
  }

  "apply wraps a value into an IO (alias for delay)" in {
    IO.apply("something").unsafeRunSync() shouldBe "something"
  }

  "suspend suspends an IO into an IO" in {
    val exception = new Exception
    IO.suspend(IO(throw exception)).attempt.unsafeRunSync() shouldBe Left(exception)
  }

  "delay wraps a value into an IO" in {
    IO.delay("something").unsafeRunSync() shouldBe "something"
  }

  "pure wraps a value into an IO" in {
    IO.pure("something").unsafeRunSync() shouldBe "something"
  }

  "fromEither raises an exception when passed in Left" in {
    val exception = new Exception
    IO.fromEither(Left(exception)).attempt.unsafeRunSync() shouldBe Left(exception)
  }

  "fromEither wraps a value into an IO when passed in Right" in {
    IO.fromEither(Right("something")).attempt.unsafeRunSync() shouldBe Right("something")
  }

  "fromOption raises an exception when passed None" in {
    val exception = new Exception
    IO.fromOption(None)(exception).attempt.unsafeRunSync() shouldBe Left(exception)
  }

  "fromOption wraps a value into an IO when passed in Some" in {
    val exception = new Exception
    IO.fromOption(Some("something"))(exception).attempt.unsafeRunSync() shouldBe Right("something")
  }

  "fromTry raises an exception when effect throws one" in {
    val exception = new Exception
    IO.fromTry(Try(throw exception)).attempt.unsafeRunSync() shouldBe Left(exception)
  }

  "fromTry wraps a value into an IO when no exception thrown" in {
    IO.fromTry(Try("something")).attempt.unsafeRunSync() shouldBe Right("something")
  }

  "none wraps None" in {
    IO.none.unsafeRunSync() shouldBe None
  }

  "raiseError throws error" in {
    val exception = new Exception
    IO.raiseError(exception).attempt.unsafeRunSync() shouldBe Left(exception)
  }

  "raiseUnless throws error unless condition is true" in {
    val exception = new Exception
    IO.raiseUnless(false)(exception).attempt.unsafeRunSync() shouldBe Left(exception)
    IO.raiseUnless(true)(exception).attempt.unsafeRunSync() shouldBe Right(())
  }

  "raiseWhen throws error when condition is true" in {
    val exception = new Exception
    IO.raiseWhen(true)(exception).attempt.unsafeRunSync() shouldBe Left(exception)
    IO.raiseWhen(false)(exception).attempt.unsafeRunSync() shouldBe Right(())
  }

  "unlessA returns unit on evaluation" in {
    IO.unlessA(true)(IO(println("something"))).unsafeRunSync() shouldBe ()
    IO.unlessA(false)(IO(println("something"))).unsafeRunSync() shouldBe ()
  }

  "whenA returns unit on evaluation" in {
    IO.whenA(true)(IO(println("something"))).unsafeRunSync() shouldBe ()
    IO.whenA(false)(IO(println("something"))).unsafeRunSync() shouldBe ()
  }

  "unit returns unit on evaluation" in {
    IO.unit.unsafeRunSync() shouldBe ()
  }

}
