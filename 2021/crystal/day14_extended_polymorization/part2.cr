require "../day.cr"

def count(times, pair, rules, hash)
  f, s = pair

  if !hash.includes?({f,s,times})
    hash[{f,s,times}] =
      (0..times)
      .accumulate ([pair]) { |pairs, i|
        if !hash.includes?({f,s,times - i})
          hash[{f,s,times-i}] =
            pairs.each.flat_map { |f1, s1|
              if rules.has_key?({f1, s1})
                c = rules[{f1,s1}]
                [{f1, c}, {c, s1}]
              else
                [{f1, s1}]
              end
          }.to_a
        end
        hash[{f,s,times-i}]
      }.last
  end
  hash[{f,s,times}]
end

Day.new(14, 2).execute { |input|
  polymer_template = input.lines[0]

  rules = Hash(Tuple(Char, Char), Char).new
  hash = Hash(Tuple(Char, Char, Int32), Array(Tuple(Char, Char))).new

  input.lines.skip(2)
    .each { |l|
      xs = l.split(" -> ")
      pair = {xs[0].chars[0], xs[0].chars[1]}
      rules[pair] = xs[1].chars[0]
    }

  rules.keys.each { |f,s|
      hash[{f,s,0}] = [{f,s}]
  }

  times = 10

  final_hash = polymer_template
    .chars
    .each
    .cons_pair
    .reduce (Hash(Char, Int32).new(0)) { |h, fs|
      f,s = fs
      h1 = count(times, {f,s}, rules, hash).map { |(_,s1)| s1 }.group_by { |x| x }.map {|k, v| {k, v.size}}
      puts h1
#      h.merge!(h1) { |_, v1, v2| v1 + v2 }
      h
    }
    .to_a.sort_by { |_,v| v }

  _, fc = final_hash.first
  _, lc = final_hash.last

  (lc-fc).to_s
}
