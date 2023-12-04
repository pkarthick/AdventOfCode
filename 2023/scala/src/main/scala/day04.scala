package day04

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(s"$currentDir/../../../../input/day04")

case class ScratchCard(
    cardNumber: Int,
    winning: List[Int],
    mine: List[Int]
):
  def winningCount: Int =
    winning.filter(num => mine.contains(num)).size

  def updateCopies(copies: Map[Int, Int]): Map[Int, Int] =
    (cardNumber + 1 to cardNumber + winningCount).foldLeft(copies)(
      (copies, card) => {

        copies.get(cardNumber) match {
          case Some(times) =>
            copies.updatedWith(card) {
              case Some(v) => Some(v + times)
              case None    => Some(1)
            }
          case None =>
            copies
        }
      }
    )

def getCards(input: String) =
  (for line <- input.linesIterator yield line match {
    case s"Card $cardNumber: $winning | $mine" => {
      ScratchCard(
        cardNumber.trim().toInt,
        winning.split(" ").filter(_.nonEmpty).map(_.trim().toInt).toList,
        mine.split(" ").filter(_.nonEmpty).map(_.trim().toInt).toList
      )
    }
  }).toList

def pow(num: Int, exponent: Int) =
  (1 until exponent).foldLeft(1) { (t, _) => t * 2 }

def part1(input: String): String =

  val cards = getCards(input)

  cards
    .foldLeft(0) { (points, sc) =>
      points +
        (if sc.winningCount == 0 then 0
         else pow(2, sc.winningCount))
    }
    .toString()

def part2(input: String): String =

  val cards = getCards(input)
  val initialCopy = (1 to cards.size).map(_ -> 1).toMap

  val copies = (1 to cards.size).foldLeft(initialCopy) {(copies, k) =>
    copies.get(k) match {
      case Some(_) => (cards(k - 1).updateCopies(copies))
      case None    => copies
    }
  }

  copies.values.sum.toString()

@main def part12: Unit =

  val input = loadInput()
  val cards = getCards(input)

  val totalPoints = cards
    .foldLeft(0) { (points, sc) =>
      points +
        (if sc.winningCount == 0 then 0
         else pow(2, sc.winningCount))
    }
  
  val initialCopy = (1 to cards.size).map(_ -> 1).toMap

  val copies = (1 to cards.size).foldLeft(initialCopy) {(copies, k) =>
    copies.get(k) match {
      case Some(_) => (cards(k - 1).updateCopies(copies))
      case None    => copies
    }
  }

  println(s"Solution for part1 is $totalPoints")
  println(s"Solution for part2 is ${copies.values.sum}")
