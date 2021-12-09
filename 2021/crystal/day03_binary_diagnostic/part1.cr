require "../day.cr"

puts Day.new(3, 1).execute { |input|
  bits_list = input
    .lines
    .map &.chars.map(&.to_i)
    .to_a

  number_of_bits = bits_list[0].size

  o, z =
    (0...number_of_bits)
    .reduce ({0,0}) { |(o, z), i|
      ones = bits_list.sum(&.[i])
      zeroes = bits_list.size-ones
      pow = 2 ** (number_of_bits - i - 1)
      ones > zeroes ? {o + pow, z} : {o, z + pow}
    }
  
  (o * z).to_s
}
