package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day5 {
  def star1(data: List[String]): Int = {
    val intersectedPoints: Map[Point, Int] = parseInput(data, true)

    intersectedPoints.collect {
      case (_, i) if i > 1 => 1
    }.sum
  }

  def star2(data: List[String]): Int = {
    val intersectedPoints: Map[Point, Int] = parseInput(data, false)

    intersectedPoints.collect {
      case (_, i) if i > 1 => 1
    }.sum
  }

  def parseInput(data: List[String], ignoreDiagonal: Boolean): Map[Point, Int] = {
    val points: List[Point] = data.foldLeft(List[Point]()) { (acc, line) =>
      val Array(source, destination) = line.split(" -> ").map(Point.apply)
      acc ++ source.to(destination, ignoreDiagonal)
    }

    points.groupBy(identity).mapValues(_.size).toMap
  }

  case class Point(x: Int, y: Int) {
    def to(other: Point, ignoreDiagonal: Boolean): List[Point] = {
      val xDiff = Math.abs(other.x - x)
      val xDir  = if x < other.x then 1 else -1
      val yDiff = Math.abs(other.y - y)
      val yDir  = if y < other.y then 1 else -1

      // This is really really ugly :-(
      if (isDiagonalTo(other)) {
        if ignoreDiagonal then return List()
        if xDiff != yDiff then throw Exception(s"Diagonal move not 45°, this should not happen: $this -> $other")

        (0 to xDiff).toList.map { d =>
          Point(x + (d * xDir), y + (d * yDir))
        }
      } else {
        val xMove = (0 to xDiff).toList.map { d =>
          Point(x + (d * xDir), y)
        }

        val yMove = (0 to yDiff).toList.map { d =>
          Point(x, y + (d * yDir))
        }

        (xMove ++ yMove).distinct
      }
    }

    def isDiagonalTo(other: Point): Boolean = x != other.x && y != other.y
  }

  object Point {
    def apply(str: String): Point = {
      val Array(x, y) = str.split(",")
      Point(x.toInt, y.toInt)
    }
  }
}
