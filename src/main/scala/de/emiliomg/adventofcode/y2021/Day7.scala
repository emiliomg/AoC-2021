package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day7 {
  def star1(data: List[String]): Long = {
    val crabs   = data.head.split(",").map(_.toInt).toList.sorted
    val minCrab = crabs.min
    val maxCrab = crabs.max

    (minCrab to maxCrab).map(getMigrationDistance(crabs)).min
  }

  def star2(data: List[String]): Long = {
    ???
  }

  def getMigrationDistance(crabs: List[Int])(position: Int): Long = {
    crabs.map(c => Math.abs(position - c)).sum
  }
}
