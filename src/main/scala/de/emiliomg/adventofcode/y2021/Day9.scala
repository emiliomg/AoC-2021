package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex
import scala.collection.mutable

object Day9 {

  type POS = (Int, Int)

  def star1(data: List[String]): Long = {
    val grid         = Grid.fromData(data)
    val lowestPoints = grid.getLowestPoints

    lowestPoints.flatMap(grid.getValueOpt).map(_ + 1).sum
  }

  def star2(data: List[String]): Long = {
    val grid         = Grid.fromData(data)
    val lowestPoints = grid.getLowestPoints

    val basins: List[List[POS]] = lowestPoints.map(grid.getBasin)
    basins.map(_.size).sorted.takeRight(3).product
  }

  case class Grid(cells: Array[Array[Int]]) {
    def getBasin(pos: POS): List[POS] = {
      def step(knownPoints: List[POS], currentPos: POS): List[POS] = {
        if (knownPoints.contains(currentPos) || (getValueOpt(currentPos) == Some(9))) return List.empty

        val checkMeOut: List[POS] = getSurroundingPoints(currentPos).diff(knownPoints)
        val newKnownPoints = checkMeOut.foldLeft(knownPoints :+ currentPos) { (known, point) =>
          (known ++ step(known, point)).distinct
        }
        newKnownPoints
      }

      step(List.empty, pos)
    }

    def getLowestPoints: List[POS] = {
      val results: List[POS] = cells.view.zipWithIndex
        .flatMap { (line, x) =>
          line.view.zipWithIndex.map { (cell, y) =>
            if (getSurroundingPoints((x, y)).flatMap(getValueOpt).forall(_ > cell)) Some((x, y))
            else None
          }
        }
        .flatten
        .toList

      results
    }

    def getValueOpt(pos: POS): Option[Int] = cells.lift(pos._1).flatMap(_.lift(pos._2))

    def getSurroundingPoints(pos: POS): List[POS] = {
      val result = List(
        (pos._1, pos._2 - 1),
        (pos._1 + 1, pos._2),
        (pos._1, pos._2 + 1),
        (pos._1 - 1, pos._2)
      ).map { p =>
        getValueOpt(p) match {
          case Some(_) => Some(p)
          case None    => None
        }
      }.flatten

      result
    }
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
