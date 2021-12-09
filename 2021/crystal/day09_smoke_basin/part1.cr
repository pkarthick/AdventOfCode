require "../day.cr"

puts Day.new(9, 1).execute { |input|
  rows = input
    .lines
    .map(&.chars.map(&.to_i))
    .to_a

  rows_count = rows.size
  cols_count = rows[0].size

  (0...rows_count).sum { |r|
    (0...cols_count).sum { |c|
      if [{r, c + 1}, {r, c - 1}, {r + 1, c}, {r - 1, c}]
        .each
        .select { |(r1, c1)| r1 >= 0 && r1 < rows_count && c1 >= 0 && c1 < cols_count }
        .all? { |(r1, c1)| rows[r][c] < rows[r1][c1] }
        rows[r][c] + 1
      else
        0
      end
    }
  }.to_s
}
