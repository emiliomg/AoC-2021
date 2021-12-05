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
      val xDiff = other.x - x
      val xStep = if x < other.x then 1 else -1
      val yDiff = other.y - y
      val yStep = if y < other.y then 1 else -1

      if (isDiagonalTo(other)) {
        if ignoreDiagonal then return List()
        List()
      } else {
        val xMove = (0 to xDiff by xStep).toList.map { d =>
          Point(x + d, y)
        }

        val yMove = (0 to yDiff by yStep).toList.map { d =>
          Point(x, y + d)
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
