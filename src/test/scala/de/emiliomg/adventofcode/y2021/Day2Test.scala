package de.emiliomg.adventofcode.y2020

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day2Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day2.star1(getTestInput) shouldEqual 150
  }

  it should "work with the puzzle input" in {
    val result = Day2.star1(getPuzzleInput)
    pprint.pprintln(s"Day2, Star1: $result")
    result shouldEqual 1488669
  }

  "Star 2" should "work with the test input" in {
    Day2.star2(getTestInput) shouldEqual 900
  }

  it should "work with the puzzle input" in {
    val result = Day2.star2(getPuzzleInput)
    pprint.pprintln(s"Day2, Star2: $result")
    result shouldEqual 1176514794
  }

  private def getTestInput: List[String] = {
    """forward 5
      |down 5
      |forward 8
      |up 3
      |down 8
      |forward 2""".stripMargin.split("\n").toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day2.txt").getLines().toList
  }
}
