package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day6 {
  def star1(data: List[String]): Long = {
    val originalFish = getFishFromData(data)
    simulateFish(originalFish, 80)
  }

  def star2(data: List[String]): Long = {
    val originalFish = getFishFromData(data)
    simulateFish(originalFish, 256)
  }

  def simulateFish(originalFish: Map[Int, Long], finalDay: Int): Long = {

    def step(fish: Map[Int, Long], day: Int): Long = {
      val newDay = day + 1
      val newFish = fish.foldLeft(Map[Int, Long]()) {
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

      if (newDay == finalDay) {
        newFish.values.sum
      } else step(newFish, newDay)
    }

    step(originalFish, 0)
  }

  def getFishFromData(data: List[String]): Map[Int, Long] =
    data.head.split(",").map(_.toInt).toList.groupBy(identity).mapValues(_.size.toLong).toMap
}
