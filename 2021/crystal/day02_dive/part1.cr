require "../day.cr"

puts Day.new(2, 1).execute { |input|

  depth, width = input
    .lines
    .map { |s|
      xs = s.split(' ')
      {xs[0], xs[1].to_i}
    }
    .reduce ({0, 0}) { |  acc, dir_dis |
      dir, dis = dir_dis
      d, w = acc
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
