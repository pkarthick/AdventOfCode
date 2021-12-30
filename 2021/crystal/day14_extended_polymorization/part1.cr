require "../day.cr"

class ListNode
  @c: Char | Nil = nil
  @next: ListNode | Nil = nil

  def initialize(@c)
  end

  def initialize()
  end

end

root = ListNode

Day.new(14, 1).execute { |input|
  polymer_template = input.lines[0]

  rules_map = Hash(Tuple(Char, Char), Char).new

  input.lines.skip(2)
    .each { |l|
      xs = l.split(" -> ")
      rules_map[{xs[0].chars[0], xs[0].chars[1]}] = xs[1].chars[0]
    }

  template =
    (0...4)
      .reduce (polymer_template.chars.each.cons_pair.to_a) { |pairs, _|
        pairs.each.flat_map { |f, s|
          if rules_map.has_key?({f, s})
            [{f, rules_map[{f, s}]}, {rules_map[{f,s}], s}]
          else
            [{f, s}]
          end
        }.to_a
      }


  chars_count = template.map { |f,s| s }.group_by { |x| x }.map { |k, v| {v.size, k} }.sort

  fc, f = chars_count.first
  lc, l = chars_count.last

  if template[0][0] == f
    fc += 1
  end

  if template[0][0] == l
    lc += 1
  end

  (lc - fc).to_s
}
