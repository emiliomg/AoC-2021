package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day4 {
  def star1(data: List[String]): Int = {
    val originalNumbers: List[Int]  = getChosenNumbers(data)
    val originalBoards: List[Board] = getBoards(data)

    def runNumbers(nextNumbers: List[Int], boards: List[Board]): Int = {
      val currentNum: Int        = nextNumbers.head
      val newBoards: List[Board] = boards.map(_.addNumber(currentNum))

      newBoards.find(_.hasWon) match {
        case Some(b) =>
          val score = b.getScore
          score * currentNum
        case None => runNumbers(nextNumbers.tail, newBoards)
      }
    }

    runNumbers(originalNumbers, originalBoards)
  }

  def star2(data: List[String]): Int = {
    ???
  }

  case class Position(number: Int, isChecked: Boolean) {
    def addNumber(addMe: Int): Position = {
      if (addMe == number) this.copy(isChecked = true) else this
    }

    def getScore: Int = if (isChecked) 0 else number
  }

  case class Board(positions: List[List[Position]]) {
    def addNumber(newNumber: Int): Board = {
      val newPositions = positions.map { line =>
        line.map(_.addNumber(newNumber))
      }
      Board(newPositions)
    }

    def hasWon: Boolean = {
      def isWon(b: List[List[Position]]): Boolean = {
        b.exists(l => l.forall(_.isChecked))
      }

      isWon(positions) || isWon(positions.transpose)
    }

    def getScore: Int = {
      positions.map { lines =>
        lines.map { p => p.getScore }.sum
      }.sum

    }
  }

  object Board {
    def fromData(data: List[String]): Board = {
      val lines = data.map(l => l.split(" ").filter(_.size > 0).toList)
      val positions = lines.map { line =>
        line.map(p => Position(p.toInt, false))
      }

      Board(positions)
    }
  }

  def getChosenNumbers(data: List[String]): List[Int] = {
    data(0).split(",").map(_.toInt).toList
  }

  def getBoards(data: List[String]): List[Board] = {
    data
      .drop(1)             // drop chosen numbers
      .grouped(6)          // pick empty starting line and board definition
      .map(l => l.drop(1)) // drop every starting line and create board
      .map(Board.fromData)
      .toList
  }
}
