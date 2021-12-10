require "../day.cr"

puts Day.new(10, 1).execute { |input|
  input
    .lines
    .sum { |l|
      st = [] of Char

      score = l.chars.each { |c|
        case c
        when '{'
          st.push('}')
        when '['
          st.push(']')
        when '<'
          st.push('>')
        when '('
          st.push(')')
        when '}'
          break 1197 unless st.pop == '}'
        when ']'
          break 57 unless st.pop == ']'
        when '>'
          break 25_137 unless st.pop == '>'
        when ')'
          break 3 unless st.pop == ')'
        else
          0
        end
      }

      score.as?(Int32) ? score : 0
    }.to_s
}
