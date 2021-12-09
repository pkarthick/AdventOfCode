require "../day.cr"

puts Day.new(9, 1).execute { |input|
  rows = input
    .lines
    .map { |s| (0..s.size - 1).map { |c| s[c].to_i }.to_a }
    .to_a

  rows_count = rows.size
  cols_count = rows[0].size

  (0..rows_count - 1).sum { |r|
    (0..cols_count - 1).sum { |c|
      if [{r, c + 1}, {r, c - 1}, {r + 1, c}, {r - 1, c}]
           .all? { |(r1, c1)|
             if r1 >= 0 && r1 < rows_count && c1 >= 0 && c1 < cols_count
               rows[r][c] < rows[r1][c1]
             else
               true
             end
           }
        rows[r][c] + 1
      else
        0
      end
    }
  }.to_s
}
