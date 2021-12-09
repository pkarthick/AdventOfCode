require "../day.cr"

def get_decimal(final_bits)
    final_bits_size = final_bits.size
    (0 ... final_bits_size).sum { |i| final_bits[i] * (2 ** (final_bits_size - i - 1)) }
end

def get_rating(bits_list : Array(Array(Int32)), bit_pos : Int32, &block : (Int32, Int32) -> Bool)
  if bits_list.size == 1
    get_decimal bits_list[0]
  else
    ones, zeroes = bits_list.partition { |bits| bits[bit_pos] == 1 }
    is_first = yield ones.size, zeroes.size
    get_rating(is_first ? ones : zeroes, bit_pos+1, &block)
  end
end

puts Day.new(3, 2).execute { |input|
  bits_list = input
    .lines
    .map(&.chars.map(&.to_i))
    .to_a

  o2_rating  = get_rating(bits_list, 0) { |ones, zeroes|
    ones >= zeroes
  }

  co2_rating  = get_rating(bits_list, 0) { |ones, zeroes|
    zeroes > ones
  }

  (o2_rating * co2_rating).to_s
}
