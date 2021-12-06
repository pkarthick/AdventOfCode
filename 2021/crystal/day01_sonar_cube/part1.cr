require "../day.cr"

puts Day.new(1, 1).execute { |input|
  input
    .lines
    .map(&.to_i)
    .each
    .cons_pair
    .count { |x, y| x < y }.to_s
}

