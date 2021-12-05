package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day5 {
  def star1(data: List[String]): Int = {
    val intersectedPoints: Map[Point, Int] = parseInput(data)

    intersectedPoints.collect {
      case (_, i) if i > 1 => 1
    }.sum
  }

  def star2(data: List[String]): Int = {
    ???
  }

  def parseInput(data: List[String]): Map[Point, Int] = {
    val points: List[Point] = data.foldLeft(List[Point]()) { (acc, line) =>
      val Array(source, destination) = line.split(" -> ").map(Point.apply)
      acc ++ source.to(destination)
    }

    points.groupBy(identity).mapValues(_.size).toMap
  }

  case class Point(x: Int, y: Int) {
    def to(other: Point): List[Point] = {
      if (x != other.x && y != other.y) return List()

      val xDiff = other.x - x
      val xStep = if x < other.x then 1 else -1
      val yDiff = other.y - y
      val yStep = if y < other.y then 1 else -1

      val xMove = (0 to xDiff by xStep).toList.map { d =>
        Point(x + d, y)
      }

      val yMove = (0 to yDiff by yStep).toList.map { d =>
        Point(x, y + d)
      }

      (xMove ++ yMove).distinct
    }
  }

  object Point {
    def apply(str: String): Point = {
      val Array(x, y) = str.split(",")
      Point(x.toInt, y.toInt)
    }
  }
}
