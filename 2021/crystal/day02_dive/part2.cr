require "../day.cr"

puts Day.new(2, 2).execute { |input|

  depth, width, _ = input
    .lines
    .map { |s|
      dir, _, dis = s.partition(' ')
      {dir, dis.to_i}
    }
    .reduce ({0, 0, 0}) { |  (d,w,a), (dir,dis) |
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
  (depth * width).to_s
}
