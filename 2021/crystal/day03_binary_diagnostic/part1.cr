require "../day.cr"

puts Day.new(3, 1).execute { |input|
  bits_list = input
    .lines
    .map &.chars.map(&.to_i)
      .to_a

  number_of_bits = bits_list[0].size

  init = [0] * number_of_bits

  ones = bits_list
    .reduce (init) { |acc, bits|
      bits.each_with_index { |bit, i| acc[i] += bit }
      acc
    }
    .to_a

  o, z = 0, 0

  ones
    .each_with_index { |c, i|
      pow = 2 ** (number_of_bits - i - 1)
      if (bits_list.size - c) < c
        o += pow
      else
        z += pow
      end
    }

  (o * z).to_s
}
