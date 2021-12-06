require "../day.cr"

puts Day.new(2, 2).execute { |input|

  depth, width, _ = input
    .lines
    .map { |s|
      xs = s.split(' ')
      {xs[0], xs[1].to_i}
    }
    .reduce ({0, 0, 0}) { |  acc, dir_dis |
      dir, dis = dir_dis
      d, w, a = acc
      if dir == "forward"
        w += dis
        d += a * dis
      elsif dir == "up"
        a -= dis
      elsif dir == "down"
        a += dis
      else
        raise "Unexpected direction in input!"
      end
      {d, w, a}
    }
  x = (depth * width).to_s
  puts x
  x
}
