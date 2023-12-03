package day03

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../../../../input/day03")

case class Position(row: Int, col: Int)
case class SchematicNumber(digits: String, position: Position)
case class Part(digits: String, position: Position):
  val size = digits.size
  val number = digits.toInt
case class Gear(position: Position, part1: Part, part2: Part):
  val ratio = part1.number * part2.number

val offsets =
  List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

case class EngineSchematic(input: String):

  val rows =
    (for line <- input.linesIterator yield {
      line.toCharArray().toVector
    }).toVector

  val height = rows.size
  val width = rows(0).size
  val numbers =
    (for r <- 0 until rows.size yield {
      val (nums, (partCol, chars)) =
        rows(r).zipWithIndex.foldLeft(
          (List.newBuilder[SchematicNumber], (-1, List[Char]()))
        ) { case ((nums, (partCol, chars)), (ch, col)) =>
          if ch.isDigit then
            (
              nums,
              if chars.isEmpty then (col, List(ch)) else (partCol, chars :+ ch)
            )
          else if chars.isEmpty then (nums, (-1, chars))
          else if chars.size > 0 then
            val part = SchematicNumber(chars.mkString, Position(r, partCol))
            (nums += part, (-1, List()))
          else (nums, (-1, List()))
        }

      if chars.size > 0 then
        val part = SchematicNumber(chars.mkString, Position(r, partCol))
        nums += part

      nums.result()
    }).flatten

  def adjacentPositions(size: Int, position: Position) =
    (0 until size)
      .flatMap(s =>
        offsets
          .map((r, c) => (position.row + r, position.col + c + s))
          .filter((r, c) => r >= 0 && c >= 0 && r < height && c < width)
      )
      .toSet

  def symbolsAroundCells(position: Position, count: Int) =
    adjacentPositions(count, position)
      .filter((r, c) => rows(r)(c) != '.' && !rows(r)(c).isDigit)
      .map((r, c) => (rows(r)(c), Position(r, c)))

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
    .map {
      case (asteriskPos, List(part1, part2)) =>
        Some(Gear(asteriskPos, part1, part2))
      case _ => None
    }
    .flatten

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
  println("both!")
