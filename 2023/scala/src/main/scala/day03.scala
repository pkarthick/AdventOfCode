package day03

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../../../../input/day03")

type Position = (Int, Int)

extension (p: Position)
  def row = p._1
  def col = p._2

// To store continuous digits
case class ContiguousDigits(digits: String, position: Position)

// To store continuous digits adjacent symbol. This is a part.
case class Part(digits: String, position: Position):
  val size = digits.size
  val number = digits.toInt

// An asterisk (*) with two adjacent parts and its location
case class Gear(position: Position, part1: Part, part2: Part):
  val ratio = part1.number * part2.number

val offsets =
  List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

case class EngineSchematic(input: String):

  val rows = input.linesIterator.map { _.toCharArray().toVector }.toVector

  val height = rows.size
  val width = rows(0).size
  val numbers =
    (for r <- 0 until rows.size yield {
      val (nums, (partCol, chars)) =
        rows(r).zipWithIndex.foldLeft(
          (List.newBuilder[ContiguousDigits], (-1, List[Char]()))
        ) { case ((nums, (partCol, chars)), (ch, col)) =>
          if ch.isDigit then
            (
              nums,
              if chars.isEmpty then (col, List(ch)) else (partCol, chars :+ ch)
            )
          else if chars.isEmpty then (nums, (-1, chars))
          else if chars.size > 0 then
            val part = ContiguousDigits(chars.mkString, (r, partCol))
            (nums += part, (-1, List()))
          else (nums, (-1, List()))
        }

      if chars.size > 0 then
        val part = ContiguousDigits(chars.mkString, (r, partCol))
        nums += part

      nums.result()
    }).flatten

  def inBounds(pos: Position) =
    pos.row >= 0 && pos.col >= 0 && pos.row < height && pos.col < width

  def isSymbol(pos: Position) =
    val (r, c) = pos
    rows(r)(c) != '.' && !rows(r)(c).isDigit

  def adjacentPositions(size: Int, position: Position) =

    val leftCellsOffset =
      List((-1, -1), (0, -1), (1, -1))
        .map((r, c) => (position.row + r, position.col + c))
     

    val topAndBottomCells =
      (0 until size)
        .flatMap(cc =>
          List(
            (position.row - 1, position.col + cc),
            (position.row + 1, position.col + cc)
          )
        )
     

    val rightCells =
      List((-1, 1), (0, 1), (1, 1))
        .map((r, c) => (position.row + r, position.col + c + size - 1))
        

    (leftCellsOffset ++ topAndBottomCells ++ rightCells).filter(inBounds).toList

  def symbolsAroundCells(position: Position, count: Int) =
    adjacentPositions(count, position)
      .filter(isSymbol)
      .map((r, c) => (rows(r)(c), (r, c)))

  val parts =
    numbers
      .filter(numInfo =>
        symbolsAroundCells(numInfo.position, numInfo.digits.size).nonEmpty
      )
      .map(numInfo => Part(numInfo.digits, numInfo.position))
      .toList

  val gears = parts
    .flatMap(part => {
      symbolsAroundCells(part.position, part.size)
        .find((ch, _) => ch == '*') match {
        case Some('*', asteriskPos) => Some(asteriskPos, part)
        case _                      => None
      }
    })
    .toSet
    .foldLeft(Map[Position, List[Part]]()) { case (m, (asteriskPos, part)) =>
      m.get(asteriskPos) match {
        case Some(nums) => m + (asteriskPos -> (part :: nums))
        case None       => m + (asteriskPos -> List(part))
      }
    }
    .flatMap {
      case (asteriskPos, List(part1, part2)) =>
        Some(Gear(asteriskPos, part1, part2))
      case _ => None
    }

def part1(input: String): String =

  val schematic = EngineSchematic(input)

  schematic.parts
    .map(_.number)
    .sum
    .toString()

def part2(input: String): String =

  val schematic = EngineSchematic(input)

  schematic.gears
    .map(_.ratio)
    .sum
    .toString()

@main def part12: Unit =

  val input = loadInput()
  val schematic = EngineSchematic(input)

  val sumOfPartNumbers = schematic.parts
    .map(_.number)
    .sum
    .toString()

  val sumOfGearRatio = schematic.gears
    .map(_.ratio)
    .sum
    .toString()

  println(s"Solution for part1 is $sumOfPartNumbers")
  println(s"Solution for part2 is $sumOfGearRatio")
