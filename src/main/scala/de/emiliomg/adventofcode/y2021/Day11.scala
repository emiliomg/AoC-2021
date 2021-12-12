package de.emiliomg.adventofcode.y2021

import scala.collection.mutable
import de.emiliomg.adventofcode.y2021.Day11.Position

object Day11 {

  def star1(data: List[String], maxSteps: Int): Long = {
    val grid   = Grid.fromData(data)
    val result = LazyList.iterate(grid)(_.step).take(maxSteps + 1)

    // result.foreach(g => pprint.pprintln(g.cells.gridView))

    result.map(_.justFlashed).sum
  }

  def star2(data: List[String]): Long = {
    val grid         = Grid.fromData(data)
    val numOfOctopus = grid.cells.size

    LazyList.iterate(grid)(_.step).indexWhere(_.justFlashed == numOfOctopus)
  }

  case class Position(x: Int, y: Int)
  type CELLS = Map[Position, Int]
  case class FlashStep(cells: CELLS, flashesToProcess: List[Position])

  case class Grid(cells: Map[Position, Int]) {

    def step: Grid = {
      val cellsIncreased: CELLS   = cells.view.mapValues(_ + 1).toMap
      val flashes: List[Position] = cellsIncreased.filter(_._2 > 9).map(_._1).toList

      def stepFlashes(fs: FlashStep): FlashStep = {
        val flashNeighbors: Set[Position] = getNeighbors(fs.flashesToProcess.head)
        val cellsIncreased: CELLS = fs.cells.map {
          case (pos, octoVal) if flashNeighbors.contains(pos) => pos -> (octoVal + 1)
          case (pos, octoVal)                                 => pos -> octoVal
        }.toMap
        val newlyFlashedNeighbors: List[Position] = flashNeighbors
          .map(p => p -> cellsIncreased(p))
          .filter(_._2 == 10)
          .map(_._1)
          .toList

        FlashStep(cellsIncreased, fs.flashesToProcess.tail ++ newlyFlashedNeighbors)
      }

      val cellsAfterFlashes: CELLS = LazyList
        .iterate(FlashStep(cellsIncreased, flashes))(stepFlashes)
        .find(_.flashesToProcess.isEmpty)
        .toList
        .last
        .cells

      val finalCells = cellsAfterFlashes.mapValues {
        case i if i > 9 => 0
        case i          => i
      }.toMap

      // pprint.pprintln(finalCells.gridView)

      Grid(finalCells)
    }

    def justFlashed: Int = cells.values.count(_ == 0)

    def getNeighbors(pos: Position): Set[Position] = {
      ((
        for {
          x <- -1 to 1
          y <- -1 to 1
        } yield Position(pos.x + x, pos.y + y)
      ).toSet - pos)
        .filter(cells.contains)
    }
  }

  object Grid {
    def fromData(data: List[String]): Grid = {
      val cells = data.zipWithIndex.flatMap { (line, x) =>
        line.split("").zipWithIndex.map { (s, y) =>
          Position(x, y) -> s.toInt
        }
      }.toMap

      Grid(cells)
    }
  }

  implicit class GridViewForMap(cells: Map[Position, Int]) {
    def gridView: Array[Array[Int]] = {
      val maxSize = cells.keySet.maxBy(_.x).x
      val cellArr = mutable.ArrayBuffer.fill(maxSize + 1, maxSize + 1)(0)

      cells.foreach { (pos, i) =>
        cellArr(pos.x)(pos.y) = i
      }

      cellArr.map(_.toArray).toArray
    }
  }
}
