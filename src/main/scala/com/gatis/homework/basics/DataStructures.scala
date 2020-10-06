package com.gatis.homework.basics

object DataStructures {

  //first attempt, imperative
  def sortConsideringEqualValuesImperative[T](map: Map[T, Int]): List[(Set[T], Int)] = {
    var reverseMap: Map[Int, Set[T]] = Map.empty
    map.foreach(x => {
      val (value, int) = x
      if (!reverseMap.contains(int)) {
        reverseMap = reverseMap + (int -> Set.empty)
      }
      reverseMap = reverseMap + (int -> (reverseMap(int) + value))
    })
    reverseMap.keySet.toList.sorted
      .map(int => {
        val set = reverseMap(int)
        (set, int)
      })
      .toList
  }

  //functional solution
  def sortConsideringEqualValues[T](map: Map[T, Int]): List[(Set[T], Int)] = map.groupBy({ case (_, int) => int }).map({ case (int, map) => (map.keySet, int) }).toList.sortBy({ case (_, int) => int })

  //functional solution using for comprehension
  def sortConsideringEqualValuesUsingForComprehension[T](map: Map[T, Int]): List[(Set[T], Int)] = (for {
    (int, map) <- map.groupBy({ case (_, int) => int })
  } yield (map.keySet, int)).toList.sortBy({ case (_, int) => int })
}
