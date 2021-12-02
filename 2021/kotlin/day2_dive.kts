import java.io.File

val day = 2
val part = 2

val testDataDir: String = System.getenv("TEST_DATA_DIR")
val file = File("""${testDataDir}/${day}/puzzle_${part}.in""")
var lines:List<String> = file.readLines()

data class Instruction(val direction: String, val distance: Int)
data class Coordinate(val width: Int = 0, val depth: Int = 0)
data class TripleCoordinate(val width: Int = 0, val depth: Int = 0, val aim: Int = 0)

fun process(c: Coordinate, ins: Instruction): Coordinate {
    return when(ins.direction) {
        "forward" -> Coordinate(c.width + ins.distance, c.depth)
        "up" -> Coordinate(c.width, c.depth - ins.distance)
        "down" -> Coordinate(c.width, c.depth + ins.distance)
        else -> Coordinate()
    }
}

fun processTriple(c: TripleCoordinate, ins: Instruction): TripleCoordinate {
    return when(ins.direction) {
        "forward" -> TripleCoordinate(c.width + ins.distance, c.depth + c.aim * ins.distance, c.aim)
        "up" -> TripleCoordinate(c.width, c.depth, c.aim - ins.distance)
        "down" -> TripleCoordinate(c.width, c.depth, c.aim + ins.distance)
        else -> TripleCoordinate()
    }
}

var coordinate = lines.map {
    val ts = it.split(' ')
    Instruction(ts[0], ts[1].toInt())
}.fold(Coordinate()) { c, i -> process(c, i) }

println(coordinate.width * coordinate.depth) // part 1 output

var coordinate1 = lines.map {
    val ts = it.split(' ')
    Instruction(ts[0], ts[1].toInt())
}.fold(TripleCoordinate()) { c, i -> processTriple(c, i) }

println(coordinate1.width * coordinate1.depth) // part 2 output
