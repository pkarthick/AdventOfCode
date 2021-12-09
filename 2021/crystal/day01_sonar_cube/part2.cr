require "../day.cr"

puts Day.new(1, 2).execute { |input|

  input
    .lines
    .map(&.to_i)
    .each_cons(3)
    .map(&.sum)
    .cons_pair
    .count { |x, y| x < y }.to_s

}

