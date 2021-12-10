require "../day.cr"

puts Day.new(10, 2).execute { |input|
  incomplete = input
    .lines
    .map { |l|
      st = [] of Char

      corrupt = l.chars.each { |c|
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
          break true if st.pop != '}'
        when ']'
          break true if st.pop != ']'
        when '>'
          break true if st.pop != '>'
        when ')'
          break true if st.pop != ')'
        else
          false
        end
      }

      if corrupt
        0
      else
        st.reverse.reduce (0_i64) { |acc, c|
          sc = case c
               when '}'
                 3
               when ']'
                 2
               when '>'
                 4
               when ')'
                 1
               else
                 raise "Unexpected!"
               end
          acc * 5 + sc
        }
      end
    }
    .sort!
    .skip_while(&.==(0))

  incomplete[incomplete.size//2].to_s
}
