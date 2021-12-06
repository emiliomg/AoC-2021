package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day6Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day6.star1(getTestInput) shouldEqual 5934
  }

  it should "work with the puzzle input" in {
    val result = Day6.star1(getPuzzleInput)
    pprint.pprintln(s"Day6, Star1: $result")
    result shouldEqual 390923
  }

  // "Star 2" should "work with the test input" in {
  //   Day6.star2(getTestInput) shouldEqual 12
  // }

  // it should "work with the puzzle input" in {
  //   val result = Day6.star2(getPuzzleInput)
  //   pprint.pprintln(s"Day6, Star2: $result")
  //   // result shouldEqual 3178
  // }

  private def getTestInput: List[String] = {
    """3,4,3,1,2""".stripMargin.split("\n").toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day6.txt").getLines().toList
  }
}
