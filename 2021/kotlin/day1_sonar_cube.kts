import java.io.File
import java.nio.file.Paths

val day = 1
val part = 2

val testDataDir: String = System.getenv("TEST_DATA_DIR")
val file = File("""${testDataDir}/${day}/puzzle_${part}.in""")
var lines:List<String> = file.readLines()

var tokens = lines.map { it.toInt() }

var count = tokens.zipWithNext().count{ it.first < it.second }
println(count)

var count1 = tokens.zipWithNext{ a, b -> a + b }.zip(tokens.drop(2) ).map { (a, b) -> a + b }.zipWithNext().count{ it.first < it.second }
println(count1)
