package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day7 {
  def star1(data: List[String]): Long = {
    val crabs   = data.head.split(",").map(_.toInt).toList.sorted
    val minCrab = crabs.min
    val maxCrab = crabs.max

    (minCrab to maxCrab)
      .map(getMigrationDistance(crabs) { (targetPosition, crab) => Math.abs(targetPosition - crab) })
      .min
  }

  def star2(data: List[String]): Long = {
    val crabs   = data.head.split(",").map(_.toInt).toList.sorted
    val minCrab = crabs.min
    val maxCrab = crabs.max

    (minCrab to maxCrab)
      .map(getMigrationDistance(crabs) { (targetPosition, crab) =>
        val d = Math.abs(targetPosition - crab)
        d * (d + 1) / 2
      })
      .min
  }

  def getMigrationDistance(crabs: List[Int])(f: (Int, Int) => Long)(position: Int): Long = {
    crabs.map(c => f(position, c)).sum
  }
}
