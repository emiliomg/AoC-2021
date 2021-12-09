package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day9Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day9.star1(getTestInput) shouldEqual 15
  }

  it should "work with the puzzle input" in {
    val result = Day9.star1(getPuzzleInput)
    pprint.pprintln(s"Day9, Star1: $result")
    result shouldEqual 504
  }

  // "Star 2" should "work with the test input" in {
  //   Day9.star2(getTestInput) shouldEqual 5353
  // }

  // it should "work with the puzzle input" in {
  //   val result = Day9.star2(getPuzzleInput)
  //   pprint.pprintln(s"Day9, Star2: $result")
  //   // result shouldEqual 1043101
  // }

  private def getTestInput: List[String] =
    """2199943210
      |3987894921
      |9856789892
      |8767896789
      |9899965678""".stripMargin.split("\n").toList

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day9.txt").getLines().toList
  }
}
