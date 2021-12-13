require "../day.cr"

def foldHorizontally(locs, fold_at_index)
  (0...fold_at_index)
    .map { |y|
      (0...locs[0].size)
        .map { |x|
          locs[y][x] || locs[fold_at_index * 2 - y][x]
        }
    }
end

def foldVertically(locs, fold_at_index)
  (0...locs.size)
    .map { |y|
      (0...fold_at_index)
        .map { |x|
          locs[y][x] || locs[y][fold_at_index * 2 - x]
        }
    }
end

def print(locs)
  locs.each { |r|
    puts r.reduce ("") { |s, c| s + (c ? '#' : ' ') }
  }
end

puts Day.new(13, 2).execute { |input|
  locs = [] of Tuple(Int32, Int32)

  maxx, maxy = 0, 0

  input
    .lines
    .each { |l|
      break if l.empty?
      x, y = l.split(',').map(&.to_i).to_a
      maxx = x if x > maxx
      maxy = y if y > maxy
      locs << ({x, y})
    }

  paper = (0..maxy).map { |y| (0..maxx).map { |x| locs.includes?({x, y}) } }

  paper = (locs.size + 1...input.lines.size)
    .accumulate(paper) { |plane, l|
      axis, line = input.lines[l].split(' ').last.split('=')
      if axis == "y"
        foldHorizontally(plane, line.to_i)
      else
        foldVertically(plane, line.to_i)
      end
    }

  print(paper.last)

  "ABKJFBGC".to_s
}
