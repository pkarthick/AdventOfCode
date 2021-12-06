TEST_DATA_DIR = Path.new(Path.home, "github/AdventOfCode/2021/testdata")

class Day
  def initialize(@day : Int8, @part : Int8)
    
  end

  def read_input(io_kind : String)
    input_path = Path.new(TEST_DATA_DIR, "#{@day}/#{io_kind}_#{@part}.in")
    File.read(input_path)
  end

  def read_output(io_kind : String)
    output_path = Path.new(TEST_DATA_DIR, "#{@day}/#{io_kind}_#{@part}.out")
    File.read(output_path)
  end

  def execute(&block : String -> String)
    input = read_input "sample"
    expected = read_output "sample"

    actual = yield input

    puts "Sample output: #{actual}"

    if actual != expected 
      raise "Actual value #{actual} is not matching the expected #{expected} value"
    end

    input = read_input "puzzle"
    
    actual = yield input

    puts "Puzzle output: #{actual}"

  end
end
