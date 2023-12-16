package day06

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int
import scala.compiletime.ops.long

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day06"
)

case class Race(time: Long, distance: Long):
  def combinations(): Long =
    val c =(1.toLong until time).count { t =>
      var speed = t
      val rem = time - t
      val traveled = rem * speed
      traveled > distance
    }
    c

def part1(input: String): String =

  val ls = input.split("\n").toList
  val times = ls(0).split(" ").filter(_.nonEmpty).drop(1).toVector
  val distances = ls(1).split(" ").filter(_.nonEmpty).drop(1).toVector

  val races =
    (0 until times.size).map(i => Race(times(i).toInt, distances(i).toInt))

  val answer = races.map(r => r.combinations()).product

  answer.toString()

def part2(input: String): String =
  
  val ls = input.split("\n").toList
  val time = ls(0).split(" ").filter(_.nonEmpty).drop(1).mkString.toLong
  val distance = ls(1).split(" ").filter(_.nonEmpty).drop(1).mkString.toLong
  
  val count = Race(time, distance).combinations()

  count.toString()

@main def part12: Unit =

  val input = loadInput()
