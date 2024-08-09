package day25

import locations.Directory.currentDir
import inputs.Input.loadFileSync
import scala.compiletime.ops.int
import scala.compiletime.ops.long
import scala.annotation.tailrec

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day25"
)

def part1(input: String): String =

  var m = Map[String, Set[String]]()
  var all = Set[String]()

  for line <- input.linesIterator do {
    line match {
      case s"$key: $connections" => {
        m += (key -> connections.split(" ").toSet)
        all ++= connections.split(" ").toSet

      }
    }
  }

  val ends = all.diff(m.keySet).toVector

  for (k, s) <- m do {
    for e <- s do {
      if m.contains(e) then {
        m += e -> (m(e) + k)
      } else {
        m += e -> Set(k)
      }
    }
  }

  // println(ends)

  var end1 = m(ends(0))
  var end2 = m(ends(1))

  var pending = m(ends(0))

  // println(end1)
  // println(end2)

  while pending.nonEmpty do {

    for elem <- pending do {
      pending -= elem
      end1 += elem

      // if (m(elem) -- end1).nonEmpty then {
      //   println(end2 -- m(elem))
      // }

      for e <- m(elem) if !end1.contains(e) do {
        pending += e
        if end2.contains(e) then {}
      }

      // println()
      // println(end1 -- end2)
      // println()

    }

  }

  val set1size = (end1 -- end2).size - 1
  val set2size = m.keySet.size - set1size

  println(set1size)
  println(set2size)

  // println((end1 -- end2).size)
  // println(m.keySet.size)

  // println(all -- end1.union(end2))

  (set1size * set2size).toString()

def part2(input: String): String =
  ""

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")
