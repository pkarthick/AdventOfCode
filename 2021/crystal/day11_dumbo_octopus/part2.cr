require "../day.cr"

def step_up_neighbours(energy_levels : Array(Array(Int32)), r : Int32, c : Int32, stepped_up : Set(Tuple(Int32, Int32)))

  if !stepped_up.includes?({r,c})

    stepped_up << {r,c}
    row_count = energy_levels.size
    col_count = energy_levels[0].size

    [{r - 1, c - 1}, {r - 1, c}, {r - 1, c + 1}, {r, c - 1}, {r, c + 1}, {r + 1, c - 1}, {r + 1, c}, {r + 1, c + 1}]
    .select { |(r, c)| (0...row_count).includes?(r) && (0...col_count).includes?(c) && !stepped_up.includes?({r, c}) }
    .each { |(r, c)|
      energy_levels[r][c] += 1
      if energy_levels[r][c] > 9
        step_up_neighbours(energy_levels, r, c, stepped_up)
      end
    }

  end

end

def step_energy_level(total_flash_count, energy_levels)
  row_count = energy_levels.size
  col_count = energy_levels[0].size

  stepped_up = Set(Tuple(Int32, Int32)).new

  (0...row_count)
    .each { |r|
      (0...col_count)
        .each { |c|
          energy_levels[r][c] += 1

          if energy_levels[r][c] > 9 && !stepped_up.includes?({r,c})
            step_up_neighbours(energy_levels, r, c, stepped_up)
          end
        }
    }

  flash_count = (0...row_count)
    .sum { |r|
      (0...col_count)
        .count { |c|

          if energy_levels[r][c] > 9
            energy_levels[r][c] = 0
            1
          else
            0
          end
        }
    }
  {total_flash_count + flash_count, energy_levels}
end

puts Day.new(11, 2).execute { |input|
  energy_levels = input
    .lines
    .map(&.chars.map(&.to_i))

  all_flashing_step = (1..)
    .reduce({0, energy_levels}) { |(a, b), step|
      count, energy_levels = step_energy_level(a, b)

      if energy_levels.all?(&.all?(&.==(0)))
        puts "All octopus are flashing on step #{step}!"
        break step
      end

      {count, energy_levels}

  }
  all_flashing_step.to_s
}
