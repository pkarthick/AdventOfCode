require "../day.cr"

def get_basin_coordinates(rows : Array(Array(Int32)), rows_count : Int32, cols_count : Int32, rc : Tuple(Int32, Int32), coordinates : Set(Tuple(Int32, Int32))) : Set(Tuple(Int32, Int32))
  r, c = rc

  [{r, c + 1}, {r, c - 1}, {r + 1, c}, {r - 1, c}]
    .each { |rc1|
      r1, c1 = rc1
      if r1 >= 0 && r1 < rows_count && c1 >= 0 && c1 < cols_count && rows[r1][c1] != 9 && rows[r1][c1] > rows[r][c]
        coordinates << rc1
        get_basin_coordinates(rows, rows_count, cols_count, rc1, coordinates)
      end
    }

  coordinates
end

def get_low_points(rows : Array(Array(Int32)), rows_count : Int32, cols_count : Int32)
  (0..rows_count - 1).flat_map { |r|
    (0..cols_count - 1).select { |c|
      [{r, c + 1}, {r, c - 1}, {r + 1, c}, {r - 1, c}]
        .all? { |rc|
          r1, c1 = rc
          if r1 >= 0 && r1 < rows_count && c1 >= 0 && c1 < cols_count
            rows[r][c] < rows[r1][c1]
          else
            true
          end
        }
    }.map { |c| {r, c} }
  }.to_a
end

puts Day.new(9, 2).execute { |input|
  rows = input
    .lines
    .map { |s| (0..s.size - 1).map { |c| s[c].to_i }.to_a }
    .to_a

  rows_count = rows.size
  cols_count = rows[0].size

  low_points = get_low_points(rows, rows_count, cols_count).to_set

  basin_counts = low_points.map { |rc| get_basin_coordinates(rows, rows_count, cols_count, rc, [rc].to_set).size }

  basin_counts.sort { |a, b| b <=> a }.each_slice(3).first.product.to_s
}
