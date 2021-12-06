require "../day.cr"

def partition_by_bit_pos(bits_list : Array(Array(Int32)), bit_pos : Int32)
  ones, zeroes = bits_list.partition { |bits| bits[bit_pos] == 1 }
  ones.size >= zeroes.size ? {ones, zeroes} : {zeroes, ones}
end

def get_decimal(bits : Array(Int32))
  dec = 0
  bits
    .each_with_index { |c, i|
      if c == 1
        dec += 2 ** (bits.size - i - 1)
      end
    }
  dec
end

puts Day.new(3, 2).execute { |input|
  bits_list = input
    .lines
    .map &.chars.map(&.to_i)
      .to_a

  o2, co2 = partition_by_bit_pos(bits_list, 0)

  bit_pos = 1
  while o2.size > 1
    o2, _ = partition_by_bit_pos(o2, bit_pos)
    bit_pos += 1
  end

  o2_rating = get_decimal(o2[0])

  bit_pos = 1
  while co2.size > 1
    _, co2 = partition_by_bit_pos(co2, bit_pos)
    bit_pos += 1
  end

  co2_rating = get_decimal(co2[0])

  (o2_rating * co2_rating).to_s
}
