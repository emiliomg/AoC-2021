package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day6 {
  def star1(data: List[String]): Int = {
    val originalFish = getFishFromData(data)

    def step(fish: Map[Int, Int], day: Int): Int = {
      val newDay = day + 1
      val newFish = fish.foldLeft(Map[Int, Int]()) {
        case (acc, (daysToSpawn, numFish)) if daysToSpawn == 0 =>
          acc
            .updatedWith(6) {
              case Some(oldVal) => Some(oldVal + numFish)
              case None         => Some(numFish)
            }
            .updatedWith(8) {
              case Some(oldVal) => Some(oldVal + numFish)
              case None         => Some(numFish)
            }
        case (acc, (daysToSpawn, numFish)) =>
          acc.updatedWith(daysToSpawn - 1) {
            case Some(oldVal) => Some(oldVal + numFish)
            case None         => Some(numFish)
          }
      }

      if (newDay == 80) {
        newFish.values.sum
      } else step(newFish, newDay)
    }

    step(originalFish, 0)
  }

  def star2(data: List[String]): Int = {
    ???
  }

  def getFishFromData(data: List[String]) =
    data.head.split(",").map(_.toInt).toList.groupBy(identity).mapValues(_.size).toMap
}
