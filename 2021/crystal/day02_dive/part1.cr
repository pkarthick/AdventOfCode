require "../day.cr"

puts Day.new(2, 1).execute { |input|

  depth, width = input
    .lines
    .map { |s|
      dir, _, dis = s.partition(' ')
      {dir, dis.to_i}
    }
    .reduce ({0, 0}) { |  (d, w), (dir, dis) |
      if dir == "forward"
        w += dis
      elsif dir == "up"
        d -= dis
      elsif dir == "down"
        d += dis
      else
        raise "Unexpected direction in input!"
      end
      {d, w}
    }
  (depth * width).to_s
}
