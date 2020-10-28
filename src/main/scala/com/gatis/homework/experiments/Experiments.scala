package com.gatis.homework.experiments
import cats._
import cats.implicits._

object Experiments {

  def parseInt(s: String): Option[Int] = Either.catchOnly[NumberFormatException](s.toInt).toOption

  def main(args: Array[String]): Unit = {
    println(Foldable[List].foldK(List(None, Option("two"), Option("three"))))
    println(Foldable[List].foldK(List(Option("three"), None, Option("two"))))
    println(Foldable[List].fold(List(None, Option("two"), Option("three"))))
    println(Foldable[List].foldK(List(List(1, 2), List(3, 4, 5))))
    println(Foldable[List].fold(List(List(1, 2), List(3, 4, 5))))

    println(MonoidK[Option].combineK(Option("hi"), Option("there")))
    println(MonoidK[Option].combineK(Option(1), Option(2)))
    println(Monoid[Option[String]].combine(Option("hi"), Option("there")))
    println(MonoidK[List].combineK(List(1, 2), List(3, 4, 5)))
    println(Monoid[List[Int]].combine(List(1, 2), List(3, 4, 5)))

    println(Foldable[List].traverse_(List("1", "b", "1"))(parseInt))

    val FoldableListOption = Foldable[List].compose[Option]
    println(FoldableListOption.fold(List(Option(1), Option(2), Option(3), Option(4))))

    val FoldableListListInt = Foldable[List].compose[List]
    println(FoldableListListInt.fold(List(List(1, 2), List(3, 4, 5))))
    println(Right(1).flatMap(_ => Right(1)))

    println(List(List(1, 2, 3), List(4, 5, 6)).flatten)
    // println(Some(1).flatten)

  }
}
