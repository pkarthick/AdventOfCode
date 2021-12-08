require "../day.cr"

puts Day.new(6, 1).execute { |input|
  age_groups = Hash(Int32, Int32).new(0)

  input
    .split(',')
    .map(&.to_i)
    .each do |age|
      age_groups[age] += 1
    end

  (1..80).each do |_|
    new_count = age_groups[0]

    (1..8).each do |age|
      age_groups[age - 1] = age_groups[age]
    end

    age_groups[8] = new_count
    age_groups[6] += new_count
  end

  puts age_groups.values.sum.to_s

  age_groups.values.sum.to_s
}
