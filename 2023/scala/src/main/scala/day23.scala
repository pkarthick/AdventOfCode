package day23

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day23"
)

def part1(input: String): String =
  val cells = input.linesIterator.map(l => l.toCharArray()).toArray
  var target = (cells.size - 1, cells(0).size - 2)
  val height = cells.size
  val width = cells(0).size
  var maxVisited: List[(Int, Int)] = List()

  def reach_target(
      rcs: List[((Int, Int), List[(Int, Int)])]
  ): Unit =
    rcs match {
      case Nil => maxVisited
      case rc :: rest => {

        if rc._1 == target then {

          if rc._2.size > maxVisited.size then {
            maxVisited = rc._2;
            reach_target(rest)
          } else {
            reach_target(rest)
          }
        } else {

          if !maxVisited.contains(rc) then {

            val (r, c) = rc._1
            val visited = rc._1 +: rc._2

            val rcs1 =
              List(
                ((r + 1, c), visited),
                ((r - 1, c), visited),
                ((r, c + 1), visited),
                ((r, c - 1), visited)
              )
                .filter { case (((rr, cc), vis)) =>
                  rr >= 0 && rr < height && cc >= 0 && cc < width
                  && (cells(rr)(cc)) != '#'
                  && (vis.length < 2 || vis.tail.head != (rr, cc))
                }

            reach_target(rcs1)

          } else {
            print(rc)
            print("")
            reach_target(rest)
          }
        }

      }
    }

  reach_target(List(((0, 1), List())))

  maxVisited.size.toString()

def part2(input: String): String =
  ""
