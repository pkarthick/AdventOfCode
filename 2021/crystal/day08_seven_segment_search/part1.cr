require "../day.cr"

puts Day.new(8, 1).execute { |input|
  input
    .lines
    .reduce (0) { |acc, line|
      line
        .split(" | ")[1].split(' ')
        .map(&.size)
        .count { |c|
          [2, 3, 4, 7].includes?(c)
        } + acc
    }.to_s
}
