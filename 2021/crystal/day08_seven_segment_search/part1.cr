require "../day.cr"

puts Day.new(8, 1).execute { |input|
  input
    .lines
    .sum { |line|
      line
        .split(" | ")[1].split(' ')
        .map(&.size)
        .count { |c| [2, 3, 4, 7].includes?(c) }
    }.to_s
}
