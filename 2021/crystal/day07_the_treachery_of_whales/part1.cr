require "../day.cr"

puts Day.new(7, 1).execute { |input|
  keys = input
    .split(',', remove_empty: true)
    .map(&.to_i)

  (0..keys.max)
    .map { |x| keys.sum(&.-(x).abs) }
    .min
    .to_s
}
