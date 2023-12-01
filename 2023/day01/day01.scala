package day01

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../input/day01")

def part1(input: String): String =

  var iter =
    for line <- input.linesIterator yield {
      val digits = line.filter(_.isDigit).map(_.toInt - 48).toList
      val num = digits.head * 10 + digits.last
      num
    }

  iter.sum.toString()

def part2(input: String): String =

  val digitStringsMap = Vector(
    "one",
    "two",
    "three",
    "four",
    "five",
    "six",
    "seven",
    "eight",
    "nine"
  ).zip(1 to 9).toMap

  val digitLiteralMap = (0 to 9).map { d => (d.toString(), d) }.toMap
  val digitsMap = digitStringsMap ++ digitLiteralMap

  var iter = for line <- input.linesIterator yield {
    val (firstDigit, restOfLine) = digitsMap.keys
      .map(d => (d, line.indexOf(d)))
      .filter { _._2 != -1 }
      .toList
      .sortBy(_(1))
      .headOption
      .map((s, i) => (digitsMap(s), line.substring(i)))
      .get

    val lastDigit = digitsMap.keys
      .map(d => (d, restOfLine.lastIndexOf(d)))
      .filter { _._2 != -1 }
      .toList
      .sortBy(_(1))
      .lastOption
      .map((s, _) => digitsMap(s))
      .get

    firstDigit * 10 + lastDigit
  }

  iter.sum.toString()
