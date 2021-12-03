package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day3Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day3.star1(getTestInput) shouldEqual 198
  }

  it should "work with the puzzle input" in {
    val result = Day3.star1(getPuzzleInput)
    pprint.pprintln(s"Day3, Star1: $result")
    // result shouldEqual 1488669
  }

  "Star 2" should "work with the test input" in {
    Day3.star2(getTestInput) shouldEqual 230
  }

  it should "work with the puzzle input" in {
    val result = Day3.star2(getPuzzleInput)
    pprint.pprintln(s"Day3, Star2: $result")
    // result shouldEqual 1176514794
  }

  private def getTestInput: List[String] = {
    """00100
      |11110
      |10110
      |10111
      |10101
      |01111
      |00111
      |11100
      |10000
      |11001
      |00010
      |01010""".stripMargin.split("\n").toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day3.txt").getLines().toList
  }
}
