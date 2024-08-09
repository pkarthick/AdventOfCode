package day17

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int
import scala.compiletime.ops.long
import scala.annotation.tailrec

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day17s"
)

enum Direction:
  case East, South, North, West

  def getTurningDirections() =
    this match {

      case East | West => {
        List(South, North)
      }

      case North | South => {
        List(East, West)
      }

    }

def createGrid(s: String): Grid =
  val cells =
    s.split('\n').map(l => l.toCharArray().map(ch => ch - '0')).toArray
  val height = cells.size
  val width = cells(0).size

  Grid(
    cells = cells,
    row = 0,
    col = 0,
    heatLoss = 0,
    direction = Direction.East,
    directionCount = 1,
    visited = List((0, 0))
  )

case class Grid(
    cells: Array[Array[Int]],
    row: Int,
    col: Int,
    heatLoss: Int,
    directionCount: Int,
    direction: Direction,
    visited: List[(Int, Int)]
):

  val height = cells.size
  val width = cells(0).size

  def copyGrid(dir: Direction, r: Int, c: Int): Grid =

    val copy = this.copy(
      directionCount = if dir == direction then directionCount + 1 else 1,
      row = r,
      col = c,
      direction = dir,
      heatLoss = heatLoss + cells(r)(c),
      visited = visited :+ ((r, c))
    )
    copy

  def nextGrids(): List[Grid] =

    val grids =
      if directionCount < 3 then direction :: direction.getTurningDirections()
      else direction.getTurningDirections()

    grids.flatMap { dir =>
      dir match {
        case Direction.East
            if col + 1 < width && !visited.contains((row, col + 1)) =>
          Some(this.copyGrid(dir, row, col + 1))
        case Direction.South
            if row + 1 < height && !visited.contains((row + 1, col)) =>
          Some(this.copyGrid(dir, row + 1, col))
        case Direction.North if row > 0 && !visited.contains((row - 1, col)) =>
          Some(this.copyGrid(dir, row - 1, col))
        case Direction.West if col > 0 && !visited.contains((row, col - 1)) =>
          Some(this.copyGrid(dir, row, col - 1))
        case _ => None
      }
    }

def moveToDestination(
    acc: List[Grid],
    heatLossMap: Map[(Int, Int), Int],
    resultMap: Map[(Int, Int), Int],
    minHeatLoss: Int
): Int =
  if acc.isEmpty then {
    minHeatLoss
  } else {
    val grid = acc.head
    if grid.row == grid.height - 1 && grid.col == grid.width - 1 then {
      if grid.heatLoss < minHeatLoss then {
        val heatLossMap1 = heatLossMap + (((grid.row, grid.col), grid.heatLoss))
        var resultMap1 = grid.visited.toList.foldLeft(resultMap)((m, rc) =>
          m + ((rc, grid.heatLoss - heatLossMap1(rc)))
        )

        moveToDestination(acc.tail, heatLossMap1, resultMap1, grid.heatLoss)
      } else {
        moveToDestination(acc.tail, heatLossMap, resultMap, minHeatLoss)
      }
    } else if grid.heatLoss < minHeatLoss then {
      val rc = (grid.row, grid.col)

      resultMap.get(rc) match
        case None =>
          heatLossMap.get(rc) match {
            case Some(heatLoss) =>
              if grid.heatLoss <= heatLoss then {
                moveToDestination(
                  grid.nextGrids() ++ acc.tail,
                  heatLossMap + ((rc, grid.heatLoss)),
                  resultMap,
                  minHeatLoss
                )
              } else {
                moveToDestination(
                  acc.tail,
                  heatLossMap,
                  resultMap,
                  minHeatLoss
                )
              }
            case None =>
              moveToDestination(
                grid.nextGrids() ++ acc.tail,
                heatLossMap + ((rc, grid.heatLoss)),
                resultMap,
                minHeatLoss
              )
          }
        case Some(lossTillTarget) =>
          if grid.heatLoss + lossTillTarget < minHeatLoss then
            val heatLossMap1 =
              heatLossMap + (((grid.row, grid.col), grid.heatLoss))
            var resultMap1 = grid.visited.toList.foldLeft(resultMap)((m, rc) =>
              m + ((rc, grid.heatLoss + lossTillTarget - heatLossMap1(rc)))
            )
            moveToDestination(
              acc.tail,
              heatLossMap1,
              resultMap1,
              grid.heatLoss + lossTillTarget
            )
          else
            moveToDestination(
              acc.tail,
              heatLossMap,
              resultMap,
              minHeatLoss
            )

    } else {
      moveToDestination(acc.tail, heatLossMap, resultMap, minHeatLoss)
    }
  }

def part1(input: String): String =
  val grid = createGrid(input)
  val heatLossMap = Map[(Int, Int), Int]() + (((0, 0), 0))
  val resultMap = Map[(Int, Int), Int]()

  val minHeatLoss =
    moveToDestination(List(grid), heatLossMap, resultMap, Int.MaxValue)
  minHeatLoss.toString()

def part2(input: String): String =
  ""

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")
