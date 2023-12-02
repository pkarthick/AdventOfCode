package day02

import locations.Directory.currentDir
import inputs.Input.loadFileSync

val input = """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"""

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../../../../input/day02")

def part1(input: String): String =

  val totalRed = 12
  val totalGreen = 13
  val totalBlue = 14

  input.linesIterator
    .map {
      case s"Game $id: $sets" => {
        val (possibleId: Int, green: Int, red: Int, blue: Int) =
          sets.split("; ").foldLeft(id.toInt, 0, 0, 0) {
            case ((id, green, red, blue), set1) =>
              set1.split(", ").foldLeft(id, green, red, blue) {
                case ((id, green, red, blue), s"$n green") =>
                  (id, if n.toInt > green then n.toInt else green, red, blue)

                case ((id, green, red, blue), s"$n red") =>
                  (id, green, if n.toInt > red then n.toInt else red, blue)

                case ((id, green, red, blue), s"$n blue") =>
                  (id, green, red, if n.toInt > blue then n.toInt else blue)

              }
          }
        if green <= totalGreen && blue <= totalBlue && red <= totalRed then {
          possibleId
        } else {
          0
        }
      }
    }
    .sum
    .toString()

def part2(input: String): String =
  input.linesIterator
    .map {
      case s"Game $id: $sets" => {
        val (_: Int, green: Int, red: Int, blue: Int) =
          sets.split("; ").foldLeft(id.toInt, 0, 0, 0) {
            case ((id, green, red, blue), set1) =>
              set1.split(", ").foldLeft(id, green, red, blue) {
                case ((id, green, red, blue), s"$n green") =>
                  (id, if n.toInt > green then n.toInt else green, red, blue)

                case ((id, green, red, blue), s"$n red") =>
                  (id, green, if n.toInt > red then n.toInt else red, blue)

                case ((id, green, red, blue), s"$n blue") =>
                  (id, green, red, if n.toInt > blue then n.toInt else blue)

              }
          }
        green * red * blue
      }
    }
    .sum
    .toString()

@main def part12: Unit =

  val input = loadInput()

  val totalRed = 12
  val totalGreen = 13
  val totalBlue = 14

  val (sumOfIds, sumOfPower) = input.linesIterator
    .foldLeft(0, 0) {
      case ((sumIds, sumOfPower), s"Game $id: $sets") => {
        val (possibleId: Int, green: Int, red: Int, blue: Int) =
          sets.split("; ").foldLeft(id.toInt, 0, 0, 0) {
            case ((id, green, red, blue), set1) =>
              set1.split(", ").foldLeft(id, green, red, blue) {
                case ((id, green, red, blue), s"$n green") =>
                  (id, if n.toInt > green then n.toInt else green, red, blue)

                case ((id, green, red, blue), s"$n red") =>
                  (id, green, if n.toInt > red then n.toInt else red, blue)

                case ((id, green, red, blue), s"$n blue") =>
                  (id, green, red, if n.toInt > blue then n.toInt else blue)

              }
          }
        if green <= totalGreen && blue <= totalBlue && red <= totalRed then {
          (sumIds + possibleId, green * red * blue + sumOfPower)
        } else {
          (sumIds, green * red * blue + sumOfPower)
        }
      }
    }

  println(s"Solution for part1 is $sumOfIds")
  println(s"Solution for part2 is $sumOfPower")
