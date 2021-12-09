package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex
import scala.collection.mutable

object Day9 {
  def star1(data: List[String]): Long = {
    val grid              = Grid.fromData(data)
    val lowestPointValues = grid.getLowestPointsValues

    // pprint.pprintln(lowestPointValues)

    lowestPointValues.map(_ + 1).sum
  }

  def star2(data: List[String]): Long = {
    ???
  }

  case class Grid(cells: Array[Array[Int]]) {
    def getLowestPointsValues: List[Int] = {
      val results: List[Int] = cells.view.zipWithIndex
        .flatMap { (line, x) =>
          line.view.zipWithIndex.map { (cell, y) =>
            // pprint.pprintln(s"$x:$y ($cell)")

            if (getSurroundingPointsValues(x, y).forall(_ > cell)) Some(cell)
            else None
          }
        }
        .flatten
        .toList

      results
    }

    def getSurroundingPointsValues(x: Int, y: Int): List[Int] = {
      val result = List(
        getValueOpt(x, y - 1),
        getValueOpt(x + 1, y),
        getValueOpt(x, y + 1),
        getValueOpt(x - 1, y)
      )

      // pprint.pprintln(s"Surr($x, $y): $result")

      result.flatten
    }

    def getValueOpt(x: Int, y: Int): Option[Int] = cells.lift(x).flatMap(_.lift(y))
  }

  object Grid {
    def fromData(data: List[String]): Grid = {
      val cells = data.map { line =>
        line.split("").map(_.toInt)
      }.toArray

      // pprint.pprintln(cells)

      Grid(cells)
    }
  }
}
