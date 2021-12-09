require "../day.cr"

puts Day.new(8, 2).execute { |input|
  input
    .lines
    .sum { |line|
      frags = line.split(" | ")
      input_segments = frags[0]
        .split(' ')
        .sort_by!(&.size)
        .map { |seg|
        (0...seg.size).map { |c| seg[c] }.sort!.to_set
        }.to_a

      one, seven, four = input_segments[0..2]
      eight = input_segments[9]

      index_of_9, rem1, rem2 =
        if four.proper_subset_of?(input_segments[6])
          {6, 7, 8}
        elsif four.proper_subset_of?(input_segments[7])
          {7, 6, 8}
        else
          {8, 6, 7}
        end

      nine = input_segments[index_of_9]
      zero, six =
        if one.proper_subset_of?(input_segments[rem1])
          {input_segments[rem1], input_segments[rem2]}
        else
          {input_segments[rem2], input_segments[rem1]}
        end

      index_of_3, rem1, rem2 =
        if one.proper_subset_of?(input_segments[3])
          {3, 4, 5}
        elsif one.proper_subset_of?(input_segments[4])
          {4, 3, 5}
        else
          {5, 3, 4}
        end

      three = input_segments[index_of_3]
      five, two =
        if (four | input_segments[rem1]).subset_of?(nine)
          {input_segments[rem1], input_segments[rem2]}
        else
          {input_segments[rem2], input_segments[rem1]}
        end

      all_sets = [zero, one, two, three, four, five, six, seven, eight, nine]

      pows = [1000, 100, 10, 1]

      frags[1]
        .split(' ')
        .map_with_index { |seg, i|
        {i, (0...seg.size).map { |c| seg[c] }.sort!.to_set}
        }
        .sum { |(powi, out_seg)|
          if digit = all_sets.index { |digit_set| out_seg == digit_set }
            digit * pows[powi]
          else
            0
          end
        }
    }.to_s
}
