package de.emiliomg.adventofcode.y2021

import scala.util.matching.Regex

object Day4 {
  def star1(data: List[String]): Int = {
    val originalNumbers: List[Int]  = getChosenNumbersFromRawData(data)
    val originalBoards: List[Board] = getBoardsFromRawData(data)

    runBoards(originalNumbers, originalBoards) match {
      case NextWinningBoard(board: Board, number: Int, _, _) :: Nil => board.getScore * number
      case Nil                                                      => throw Exception("No winning board :-(")
      case omg => throw Exception(s"Got more than one final winning board: $omg")
    }
  }

  def star2(data: List[String]): Int = {
    val originalNumbers: List[Int]  = getChosenNumbersFromRawData(data)
    val originalBoards: List[Board] = getBoardsFromRawData(data)

    def step(numbers: List[Int], boards: List[Board]): Int = {
      runBoards(numbers, boards) match {
        case NextWinningBoard(winBoard, winNumber, _, Nil) :: Nil =>
          winBoard.getScore * winNumber
        case NextWinningBoard(_, wn, leftoverNumbers, leftoverBoards) :: _ =>
          step(leftoverNumbers, leftoverBoards)
        case omg => throw Exception(s"This should not happen: $omg")
      }
    }

    step(originalNumbers, originalBoards)
  }

  case class NextWinningBoard(
    winningBoard: Board,
    winningNumber: Int,
    leftoverNumbers: List[Int],
    leftoverBoards: List[Board]
  )

  def runBoards(nextNumbers: List[Int], boards: List[Board]): List[NextWinningBoard] = {
    val currentNum: Int        = nextNumbers.head
    val newBoards: List[Board] = boards.map(_.addNumber(currentNum))

    newBoards.filter(_.hasWon) match {
      case winningBoards if winningBoards.size > 0 =>
        val leftoverBoards = newBoards.diff(winningBoards)
        winningBoards.map { b =>
          NextWinningBoard(b, currentNum, nextNumbers.tail, leftoverBoards)
        }

      case Nil => runBoards(nextNumbers.tail, newBoards)
      case omg => throw Exception(s"This should not happen $omg")
    }
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

  def getChosenNumbersFromRawData(data: List[String]): List[Int] = {
    data(0).split(",").map(_.toInt).toList
  }

  def getBoardsFromRawData(data: List[String]): List[Board] = {
    data
      .drop(1)             // drop chosen numbers
      .grouped(6)          // pick empty starting line and board definition
      .map(l => l.drop(1)) // drop every starting line and create board
      .map(Board.fromData)
      .toList
  }
}
