package com.gatis.homework.hw10

object Homework10 {
  /*
  For the homework I decided to write about the Monad type class. I chose it as I had heared the term
  many times before, but until the bootcamp, had not used in practice since my background has involved
  primarily the OOP paradigm. Being curious I had looked the term up, though by reading it's definition
  in the context of category theory as it often is presented, did not give me much of a clue of what it
  is and how to use it. With this homework submission I hope to make it cleared for both myself as well
  as other students. This will focus on the Cats library implementation.

  This type class expands on the functionality offered by a number of case classes it extends:
  Functor (provides the *map* method: map[A, B](fa: F[A])(f: A => B): F[B])
  Apply (adds the *ap* method, which is similar to map, but with a different signature:
  ap[A, B](fa: F[A])(f: F[A => B]): F[B])
  Applicative (adds the *pure* method: pure[A](a: A): F[A]).

  These type classes provide more than just these methods, but I would consider these the more important ones,
  as the others are derived.

  Monad adds to these the *flatten* method: flatten[A](x: F[F[A]]): F[A]
  From the method signature we may see the intuition behind this function: it takes the value from the nested
  parameterized type F and returns in the same parameterized type F, but no longer nested twice.

  Combining this with *map* gives us the useful *flatMap* method we have seen often in the lectures, which is
  also necessary for for-comprehensions.
   */

}
