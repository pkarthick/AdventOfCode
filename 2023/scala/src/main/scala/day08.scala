package day08

import locations.Directory.currentDir
import inputs.Input.loadFileSync

@main def part1: Unit =
  println(s"The solution is ${part1(loadInput())}")

@main def part2: Unit =
  println(s"The solution is ${part2(loadInput())}")

def loadInput(): String = loadFileSync(
  s"$currentDir/../../../../input/day08"
)

enum Direction:
  case Left, Right

case class Node(node: String, leftNode: String, rightNode: String)

case class DesertMap(
    directions: List[Direction],
    nodes: List[Node],
    nodesMap: Map[String, Node]
)

def createMap(input: String): DesertMap =

  val lines = input.linesIterator.toVector

  val dirs = lines(0)

  val nodes = lines
    .drop(2)
    .map { case s"$node = ($leftNode, $rightNode)" =>
      Node(node, leftNode, rightNode)
    }
    .toList

  val nodesMap = nodes.map { node =>
    (node.node, node)
  }.toMap

  val directions = dirs
    .toCharArray()
    .map {
      case 'L' => Direction.Left
      case 'R' => Direction.Right
    }
    .toList

  DesertMap(directions, nodes, nodesMap)

def part1(input: String): String =

  val dm = createMap(input)

  val (count, _, _) = traverseMap(dm, "AAA", "AAA", "ZZZ", dm.directions, 0)

  count.toString()

def traverseMap(
    dm: DesertMap,
    node: String,
    startNode: String,
    destNode: String,
    directions: List[Direction],
    count: Int
): (Int, String, List[Direction]) =
  directions match {
    case Nil => traverseMap(dm, node, startNode, destNode, dm.directions, count)
    case direction :: _ =>
      dm.nodesMap(node) match {

        case Node(dn, leftNode, rightNode) if dn == destNode =>
          (count, dn, directions.tail)
        case Node(node, leftNode, rightNode) =>
          direction match {
            case Direction.Left =>
              traverseMap(
                dm,
                leftNode,
                startNode,
                destNode,
                directions.tail,
                count + 1
              )
            case Direction.Right =>
              traverseMap(
                dm,
                rightNode,
                startNode,
                destNode,
                directions.tail,
                count + 1
              )
          }
      }
  }

def traverseMap1(
    dm: DesertMap,
    oldState: (Int, String, List[Direction]),
    nodeState: (Int, String, List[Direction]),
    m: Map[(String, Int), (String, Int)],
    targetCount: Int
): (Map[(String, Int), (String, Int)], (Int, String, List[Direction])) =
  if nodeState._1 >= targetCount
    && nodeState._2.endsWith("Z")
  then (m, nodeState)
  else

    val (count, node, directions1) = nodeState
    val directions = if directions1.isEmpty then dm.directions else directions1

    m.get((node, directions.size)) match
      case Some(c) if c._2 > 0 =>
        val diff = targetCount - count
        val timesToAdd = diff / c._2 + (if diff % c._2 == 0 then 0 else 1)
        (
          m,
          (
            nodeState._1 + timesToAdd * c._2,
            c._1,
            dm.directions.drop(directions.size)
          )
        )

      case _ =>
        directions match {
          case Nil =>
            traverseMap1(
              dm,
              oldState,
              (nodeState._1, nodeState._2, dm.directions),
              m,
              targetCount
            )
          case direction :: _ =>
            dm.nodesMap(node) match {
              case Node(dn, leftNode, rightNode)
                  if dn.endsWith("Z")  => //  && count >= targetCount
                (
                  m + ((
                    oldState._2,
                    directions.size
                  ) -> (dn, count - oldState._1)),
                  (count, dn, directions.tail)
                )

              case Node(node, leftNode, rightNode) =>
                direction match {
                  case Direction.Left =>
                    traverseMap1(
                      dm,
                      oldState,
                      (count + 1, leftNode, directions.tail),
                      m,
                      targetCount
                    )
                  case Direction.Right =>
                    traverseMap1(
                      dm,
                      oldState,
                      (count + 1, rightNode, directions.tail),
                      m,
                      targetCount
                    )
                }
            }
        }

def simulataneousTraversal(
    dm: DesertMap,
    allStates: List[(Int, String, List[Direction])],
    nodeStates: List[(Int, String, List[Direction])],
    m: Map[(String, Int), (String, Int)],
    targetCount: Int
): Int =

  val res = (2 to nodeStates.size).foldLeft(true, 2) {case ((all, same), n) =>
    val set = (0 until n).map(i => nodeStates(i)._1).toSet
    if set.size == 1 && all && set.head != 0 then (all && true, n)
    else if all then (false, n)
    else (false, same)
  }

//   if res._1 && res._2 == nodeStates.size then 
//     simulataneousTraversal(
//     dm,
//     nodeStates: List[(Int, String, List[Direction])],
//     m: Map[(String, Int), (String, Int)],
//     targetCount: Int
// )

//   else 
    
  if res._1 && res._2 == dm.nodes.size then nodeStates(0)._1
  else

    

    val targetCount1 = nodeStates.take(res._2).map(r => r._1).max

    val results: (
        Map[(String, Int), (String, Int)],
        List[(Int, String, List[Direction])]
    ) = nodeStates.take(res._2)
      .foldLeft((m, List[(Int, String, List[Direction])]())) {
        case ((m, states), state) =>
          if state._1 == 0 || state._1 < targetCount1 then
            val (m1, newState) = traverseMap1(dm, state, state, m, targetCount1)
            (m1, newState :: states)
          else (m, state :: states)
      }

    val set = results._2.map(_._1).toSet
    val list = results._2.map(_._1).toList

    println(set)
    
    if set.size == 1 && set.head != 0 && results._2.forall(r =>
        r._2.endsWith("Z")
      )
    then 
      if results.size == dm.nodes.size then
        set.head
      else
        val newStates = results._2 :+ allStates(results.size)
        val allStates1 = results._2 ++ allStates.drop(results.size)
        simulataneousTraversal(dm, allStates1, newStates, m, set.head)
    else 
      simulataneousTraversal(dm, allStates, results._2, results._1, set.max)


def part2(input: String): String =

//   val input = """LR

// 11A = (11B, XXX)
// 11B = (XXX, 11Z)
// 11Z = (11B, XXX)
// 22A = (22B, XXX)
// 22B = (22C, 22C)
// 22C = (22Z, 22Z)
// 22Z = (22B, 22B)
// XXX = (XXX, XXX)"""

  val dm = createMap(input)

  val as = dm.nodesMap.keySet.filter(k => k.endsWith("A")).toList

  val inputs = as.map(n => (0, n, dm.directions)).toList

  val x =
    simulataneousTraversal(dm, inputs, inputs, Map[(String, Int), (String, Int)](), 0)

  x.toString()

@main def part12: Unit =

  val input = loadInput()
