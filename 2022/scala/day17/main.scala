import scala.util.boundary
import scala.annotation.tailrec

case class Row(
    cells: Vector[Boolean] =
      Vector(false, false, false, false, false, false, false)
)

case class RockShape(cells: Vector[Vector[Boolean]]):
  val height = cells.size
  val width = cells(0).size

val horizontalShape = RockShape(Vector(Vector(true, true, true, true)))
val plusShape = RockShape(
  Vector(
    Vector(false, true, false),
    Vector(true, true, true),
    Vector(false, true, false)
  )
)
val invertedLShape = RockShape(
  Vector(
    Vector(false, false, true),
    Vector(false, false, true),
    Vector(true, true, true)
  )
)
val verticalShape =
  RockShape(Vector(Vector(true), Vector(true), Vector(true), Vector(true)))
val squareShape = RockShape(Vector(Vector(true, true), Vector(true, true)))

val allRockShapes =
  Vector(
    horizontalShape,
    plusShape,
    invertedLShape,
    verticalShape,
    squareShape
  )

val rockCount = 2022
val maxWidth = 7

enum Direction:
  case Left, Right

case class Position(row: Int = 0, col: Int = 2)

case class Chamber(rows: Vector[Row] = Vector())

case class Rock(shape: RockShape, position: Position)

case class ChamberState(
    chamber: Chamber,
    rock: Rock
)

def canPush(
    state: ChamberState,
    position: Position
): Option[Position] =

  val Position(row, col) = position

  val ok =
    (0 until state.rock.shape.height).forall(r =>
      (0 until state.rock.shape.width).forall(c =>
        !state.rock.shape.cells(r)(c)
          || !state.chamber.rows(row + r).cells(col + c)
      )
    )

  if ok then Some(position) else None

def pushLeft(state: ChamberState): Option[Position] =

  val Position(row, col) = state.rock.position

  if col > 0 then {
    canPush(state, Position(row, col - 1))
  } else {
    None
  }

def pushRight(state: ChamberState): Option[Position] =

  val Position(row, col) = state.rock.position

  if col + state.rock.shape.width < maxWidth then {
    canPush(state, Position(row, col + 1))
  } else {
    None
  }

def fallsDown(state: ChamberState): Option[Position] =

  val Position(row, col) = state.rock.position

  if row + state.rock.shape.height < state.chamber.rows.size then {
    canPush(state, Position(row + 1, col))
  } else {
    None
  }

def addEmptyRows(state: ChamberState) =
  val emptyRowsCount = state.chamber.rows.filter { _.cells.forall { !_ } }.size
  val requiredCount = 3 + state.rock.shape.height
  val diff = requiredCount - emptyRowsCount

  if diff > 0 then
    val emptyRows = (0 until diff).map(_ => Row()).toVector
    ChamberState(
      Chamber(emptyRows ++ state.chamber.rows),
      state.rock
    )
  else
    val Position(row, col) = state.rock.position
    val rock = state.rock.copy(position = Position(row - diff, col))
    ChamberState(state.chamber, rock)

def patchFloater(state: ChamberState) =

  var Position(rowIndex, colIndex) = state.rock.position

  val newRows =
    for r <- 0 until state.chamber.rows.size yield {
      if r >= rowIndex && r <= rowIndex + state.rock.shape.height - 1 && r - rowIndex >= 0 && r - rowIndex <= state.rock.shape.height
      then
        val cells =
          for c <- 0 until maxWidth yield {
            if c >= colIndex && c <= colIndex + state.rock.shape.width - 1
              && c - colIndex >= 0 && c - colIndex <= state.rock.shape.width
            then state.rock.shape.cells(r - rowIndex)(c - colIndex)
            else state.chamber.rows(r).cells(c)
          }
        Row(cells.toVector)
      else state.chamber.rows(r)
    }

  val newChamber = Chamber(newRows.toVector)
  ChamberState(chamber = newChamber, state.rock)

def processState(
    state: ChamberState,
    allDirections: Vector[Direction],
    directionIndex: Int
): (ChamberState, Int) =

  val floatShape =
    if allDirections(directionIndex) == Direction.Left then pushLeft
    else pushRight

  val afterFloatState = floatShape(state) match {
    case Some(newPosition) => state.copy(rock = state.rock.copy(position = newPosition))
    case None              => state
  }

  val nextIndex = (directionIndex + 1) % allDirections.size

  fallsDown(afterFloatState) match {
    case None => (patchFloater(afterFloatState), nextIndex)
    case Some(newPosition) => {
      val afterDownState = afterFloatState.copy(rock = state.rock.copy(position = newPosition))
      processState(
        afterDownState,
        allDirections,
        nextIndex
      )
    }
  }

def part1(input: String): Unit =

  val chars: Seq[Char] = input
  val allDirections = chars
    .map(c => if c == '>' then Direction.Right else Direction.Left)
    .toVector

  val state = ChamberState(Chamber(), Rock(allRockShapes(0), Position()))
  val directionIndex = 0
  val (ChamberState(finalChamber, _), _) =
    (0 until 2022).foldLeft((state, directionIndex))((tuple, rockCount) => {

      val (ChamberState(chamber, _), directionIndex) = tuple

      val newState =
        ChamberState(
          chamber,
          Rock(allRockShapes(rockCount % allRockShapes.size), Position())
        )

      processState(addEmptyRows(newState), allDirections, directionIndex)

    })

  println(finalChamber.rows.dropWhile { _.cells.forall { !_ } }.size)

@main def main() =
  val input = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
  part1(input)
