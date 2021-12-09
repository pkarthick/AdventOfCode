require "../day.cr"

struct Line
  def initialize(@start : Tuple(Int32, Int32), @finish : Tuple(Int32, Int32))
  end

  def get_delta(x : Int32, y : Int32)
    if x == y
      0
    elsif x < y
      1
    else
      -1
    end
  end

  def points
    sx, sy = @start
    fx, fy = @finish

    xd = get_delta(sx, fx)
    yd = get_delta(sy, fy)

    pts = [] of Tuple(Int32, Int32)

    if sx == fx || sy == fy
      while sx != fx || sy != fy
        pts << {sx, sy}
        sx += xd
        sy += yd
      end
      pts << {fx, fy}
    end

    pts
  end

  def Line.from(l : String)
    pts = l.split(" -> ").map(&.split(',').map(&.to_i))
    sx, sy = pts[0][0], pts[0][1]
    fx, fy = pts[1][0], pts[1][1]
    Line.new({sx, sy}, {fx, fy})
  end
end

puts Day.new(5, 1).execute { |input|
  input
    .lines
    .flat_map { |l| (Line.from l).points }
    .group_by { |x| x }
    .map { |_, v| v.size }
    .count { |c| c > 1 }
    .to_s
}
