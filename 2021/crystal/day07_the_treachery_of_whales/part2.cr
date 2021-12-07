require "../day.cr"

puts Day.new(7, 2).execute { |input|
  keys = input
    .split(',', remove_empty: true)
    .map(&.to_i)

  max = keys.max
  costs = (0..max).accumulate

  (0..max)
    .map { |x|
      keys.reduce (0) { |a, y|
        d = (x - y).abs
        a + costs[d]
      }
    }
    .min
    .to_s
}
