package com.gatis.homework.effects

import scala.concurrent.Future
import scala.util.Try
import scala.annotation.tailrec
import scala.util.control.NonFatal

/*
 * Homework 1. Provide your own implementation of a subset of `IO` functionality.
 *
 * Provide also tests for this functionality in EffectsHomework1Spec (which you should create).
 *
 * Refer to:
 *  - https://typelevel.org/cats-effect/datatypes/io.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO$.html
 *  - https://typelevel.org/cats-effect/api/cats/effect/IO.html
 * about the meaning of each method as needed.
 *
 * There are two main ways how to implement IO:
 * - Executable encoding  - express every constructor and operator for our model in terms of its execution
 * - Declarative encoding - express every constructor and operator for our model as pure data in a recursive
 *                          tree structure
 *
 * While the real Cats Effect IO implementation uses declarative encoding, it will be easier to solve this
 * task using executable encoding, that is:
 *  - Add a `private val run: () => A` parameter to the class `IO` private constructor
 *  - Have most of the methods return a `new IO(...)`
 *
 * Ask questions in the bootcamp chat if stuck on this task.
 */
object EffectsHomework1Declarative {
  implicit val ec = scala.concurrent.ExecutionContext.global

  final private case class Pure[A](a: A) extends IO[A]
  final private case class Delay[A](thunk: () => A) extends IO[A]
  final private case class RaiseError[A](e: Throwable) extends IO[A]
  final private case class FlatMap[A, B](source: IO[A], f: A => IO[B]) extends IO[B]
  final private case class Map[A, B](source: IO[A], f: A => B) extends IO[B]

  sealed trait IO[A] {
    def run(): A = evaluate(this)

    @tailrec
    private def evaluate(value: IO[A]): A = (value match {
      case Pure(a)            => a
      case Delay(thunk)       => evaluate(Pure(thunk())) // will just throw in case of exception, so no need to wrap in RaiseError
      case RaiseError(e)      => throw e
      case FlatMap(source, f) => evaluate(f(source.run()))
      case Map(source, f)     => evaluate(Pure(f(source.run())))
    })

    //I don't believe this is actually a stack safe implementation I'll be interested in feedback how I could transform this so it would be stack safe.

    def map[B](f: A => B): IO[B] = Map(this, f)
    def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(this, f)
    def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
    def as[B](newValue: => B): IO[B] = map(_ => newValue)
    def void: IO[Unit] = map(_ => ())
    def attempt: IO[Either[Throwable, A]] = IO(Try(run()).toEither)
    def option: IO[Option[A]] = attempt.map(_.toOption)
    def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = attempt.flatMap(_.fold(f, IO(_)))
    def redeem[B](recover: Throwable => B, map: A => B): IO[B] = attempt.map(_.fold(recover, map))
    def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = attempt.flatMap(_.fold(recover, bind))
    def unsafeRunSync(): A = run()
    def unsafeToFuture(): Future[A] = Future { run() }
  }

  object IO {
    def apply[A](body: => A): IO[A] = delay(body)
    def suspend[A](thunk: => IO[A]): IO[A] = IO(thunk.unsafeRunSync())
    def delay[A](body: => A): IO[A] = Delay(() => body)
    def pure[A](a: A): IO[A] = Pure(a)
    def fromEither[A](e: Either[Throwable, A]): IO[A] = e.fold(raiseError, pure)
    def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option.fold(raiseError(orElse): IO[A])(pure)
    def fromTry[A](t: Try[A]): IO[A] = t.fold(raiseError, pure)
    def none[A]: IO[Option[A]] = pure(None)
    def raiseError[A](e: Throwable): IO[A] = RaiseError(e)
    def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = unlessA(cond)(raiseError(e))
    def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = whenA(cond)(raiseError(e))
    def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else action
    def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) action else unit
    val unit: IO[Unit] = pure(())
  }
}