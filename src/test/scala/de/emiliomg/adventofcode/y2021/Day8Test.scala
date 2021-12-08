package de.emiliomg.adventofcode.y2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.io.Source

class Day8Test extends AnyFlatSpec with Matchers {
  "Star 1" should "work with the test input" in {
    Day8.star1(getTestInput) shouldEqual 26
  }

  it should "work with the puzzle input" in {
    val result = Day8.star1(getPuzzleInput)
    pprint.pprintln(s"Day8, Star1: $result")
    result shouldEqual 445
  }

  "Star 2" should "work with the small test input" in {
    Day8.star2(getSmallTestInput) shouldEqual 5353
  }

  it should "work with the large test input" in {
    Day8.star2(getTestInput) shouldEqual 61229
  }

  it should "work with the puzzle input" in {
    val result = Day8.star2(getPuzzleInput)
    pprint.pprintln(s"Day8, Star2: $result")
    result shouldEqual 1043101
  }

  private def getSmallTestInput: List[String] = List(
    "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
  )

  private def getTestInput: List[String] = {
    """be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
      |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
      |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
      |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
      |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
      |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
      |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
      |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
      |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
      |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce""".stripMargin
      .split("\n")
      .toList
  }

  private def getPuzzleInput: List[String] = {
    Source.fromResource("day8.txt").getLines().toList
  }
}
