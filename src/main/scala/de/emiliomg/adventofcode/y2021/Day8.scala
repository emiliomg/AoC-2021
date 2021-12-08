package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

/*
1 => 2
4 => 4
7 => 3
8 => 7
 */
object Day8 {
  def star1(data: List[String]): Long = {
    data.map { entry =>
      val Array(_, digitStr) = entry.split("\\|").map(_.trim)
      val digits             = digitStr.split(" ").map(_.trim)
      digits.filter(digit => List(2, 4, 3, 7).contains(digit.size)).size
    }.sum
  }

  def star2(data: List[String]): Long = ???
}
