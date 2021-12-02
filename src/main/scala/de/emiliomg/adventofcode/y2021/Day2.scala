package de.emiliomg.adventofcode.y2020

import scala.util.matching.Regex

object Day2 {

  val forward: Regex = "forward (\\d+)".r
  val up: Regex      = "up (\\d+)".r
  val down: Regex    = "down (\\d+)".r

  def star1(data: List[String]): Int = {
    case class Position(depth: Int, horizontal: Int)
    val finalPos: Position = data.foldLeft(Position(0, 0)) { (acc: Position, step: String) =>
      step match {
        case forward(dist) => acc.copy(horizontal = acc.horizontal + dist.toInt)
        case down(dist)    => acc.copy(depth = acc.depth + dist.toInt)
        case up(dist)      => acc.copy(depth = acc.depth - dist.toInt)
        case _             => throw Exception(s"Unknown step: $step")
      }
    }

    finalPos.depth * finalPos.horizontal
  }

  def star2(data: List[String]): Int = {
    case class Submarine(aim: Int, depth: Int, horizontal: Int)
    val finalPos: Submarine = data.foldLeft(Submarine(0, 0, 0)) { (acc: Submarine, step: String) =>
      step match {
        case forward(dist) =>
          acc.copy(horizontal = acc.horizontal + dist.toInt, depth = acc.depth + (acc.aim * dist.toInt))
        case down(dist) => acc.copy(aim = acc.aim + dist.toInt)
        case up(dist)   => acc.copy(aim = acc.aim - dist.toInt)
        case _          => throw Exception(s"Unknown step: $step")
      }
    }

    finalPos.depth * finalPos.horizontal
  }
}
