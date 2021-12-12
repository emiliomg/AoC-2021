package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day11Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the small test input for 2 days" in {
    Day11.star1(getSmallTestInput, 2) shouldEqual 9
  }

  it should "work with the large test input for 10 days" in {
    Day11.star1(getLargeTestInput, 10) shouldEqual 204
  }

  it should "work with the large test input for 100 days" in {
    Day11.star1(getLargeTestInput, 100) shouldEqual 1656
  }

  it should "work with the puzzle input" in {
    val result = Day11.star1(getPuzzleInput, 100)
    pprint.pprintln(s"Day11, Star1: $result")
    result shouldEqual 1655
  }

  "Star 2" should "work with the test input" in {
    Day11.star2(getLargeTestInput) shouldEqual 195
  }

  it should "work with the puzzle input" in {
    val result = Day11.star2(getPuzzleInput)
    pprint.pprintln(s"Day11, Star2: $result")
    result shouldEqual 337
  }

  private def getSmallTestInput: List[String] =
    """11111
      |19991
      |19191
      |19991
      |11111""".stripMargin.split("\n").toList

  private def getLargeTestInput: List[String] =
    """5483143223
      |2745854711
      |5264556173
      |6141336146
      |6357385478
      |4167524645
      |2176841721
      |6882881134
      |4846848554
      |5283751526""".stripMargin.split("\n").toList

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day11.txt").getLines().toList
  }
}
