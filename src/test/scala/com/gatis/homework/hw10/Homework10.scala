package com.gatis.homework.hw10

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import cats._

class Homework10 extends AnyFreeSpec with Matchers {
  /*
  For the homework I decided to write about the Monad type class. I chose it as I had heared the term
  many times before, but until the bootcamp, had not used in practice since my background has involved
  primarily the OOP paradigm. Being curious I had looked the term up, though by reading it's definition
  in the context of category theory as it often is presented, did not give me much of a clue of what it
  is and how to use it. With this homework submission I hope to make it cleared for both myself as well
  as other students. This will focus on the Cats library implementation.

  - This type class expands on the functionality offered by a number of case classes it extends:
  - Functor (provides the *map* method: map[A, B](fa: F[A])(f: A => B): F[B])
  - Apply (adds the *ap* method, which is similar to map, but with a different signature:
      ap[A, B](f: F[A => B])(fa: F[A]): F[B]
  - Applicative (adds the *pure* method: pure[A](a: A): F[A]).

  A few illustrative examples:
   */

  "Functor" - {
    "defines map" in {
      Functor[List].map(List(1, 2, 3))(_.toString) should be(List("1", "2", "3"))
      Functor[Option].map(Some("abc"))(_.length) should be(Some(3))
    }
  }

  "Apply" - {
    "defines ap" in {
      def intToString: Int => String = _.toString
      def toUpperCase: String => String = _.toUpperCase

      Apply[List].ap(List(intToString))(List(1, 2, 3)) should be(List("1", "2", "3"))

      Apply[Option].ap(Some(toUpperCase))(Option("abc")) should be(Some("ABC"))
    }
  }

  "Applicative" - {
    "defines pure" in {
      Applicative[List].pure(1) should be(List(1))

      Applicative[Option].pure("abc") should be(Some("abc"))
    }
  }

  /*
  These type classes provide more than just these methods, but I would consider these the more important
  ones, as the others are derived.

  Monad adds to these the *flatten* method: flatten[A](x: F[F[A]]): F[A]
  From the method signature we may see the intuition behind this function: it takes the value from the
  nested parameterized type F and returns in the same parameterized type F, but no longer nested twice.

  Combining this with *map* gives us the useful *flatMap* method: flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  We have seen it often in the lectures, and used it even more, as for-comprehensions are syntactic sugar
  for nested *map* nested within *flatMap* applications.
   */

  "Monad" - {
    "defines flatten" in {
      Monad[List].flatten(List(List(1, 2, 3), List(6, 7))) should be(List(1, 2, 3, 6, 7))
      Monad[List].flatten(List(Nil)) should be(Nil)

      Monad[Option].flatten(Option(Some("abc"))) should be(Some("abc"))
      Monad[Option].flatten(Option(None)) should be(None)
    }

    "we can derive flatMap from *map* and *flatten*" in {
      def intToSomeIntPair(x: Int): List[Int] = List(x, 2 * x)
      val list = List(1, 2, 3)
      val list2 = List(10, 20)

      Monad[List].flatten(Functor[List].map(list)(intToSomeIntPair)) should be(List(1, 2, 2, 4, 3, 6))
      // same as
      Monad[List].flatMap(list)(intToSomeIntPair) should be(List(1, 2, 2, 4, 3, 6))

      Monad[List].flatMap(list)(n1 => Functor[List].map(list2)(n2 => n1 + n2)) should
        be(List(11, 21, 12, 22, 13, 23))
      // same as
      (for {
        n1 <- list
        n2 <- list2
      } yield n1 + n2) should be(List(11, 21, 12, 22, 13, 23))
      /* Perhaps in this short example, the for comprehension is not much cleaner, but for deeper nesting
         it quickly shows the convenience we get for code readabilty.
       */
    }
  }

  /* I hope this brief overview of the Monad typeclass was of some value to the reader, if only to solidify
     and reaffirm knowledge of concepts already explored in the lectures.
   */

}
