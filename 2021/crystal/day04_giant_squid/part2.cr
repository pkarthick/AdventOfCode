require "../day.cr"

class Square
  def initialize(@number : Int32, @marked = false)
  end

  def number
    @number
  end

  def marked
    @marked
  end

  def mark
    @marked = true
  end
end

class Board
  def initialize(@rows : Array(Array(Square)))
    @completed = false
  end

  def check_rows
    @rows.any?(&.all?(&.marked))
  end

  def check_cols
    col_count = @rows[0].size
    (0..col_count - 1).any? { |col| @rows.all?(&.[col].marked) }
  end

  def unmarked_total
    @rows.flatten.reduce (0) { |a, sq| sq.marked ? a : a + sq.number }
  end

  def mark_square(number)
    if square = @rows.flatten.find { |sq| sq.@number == number }
      square.mark
      if !@completed
        @completed = check_rows || check_cols
      end
    end
    @completed
  end
end

puts Day.new(4, 2).execute { |input|
  lines = input.lines.each.select { |l| !l.empty? }.to_a

  numbers = lines[0]
    .split(',', remove_empty: true)
    .map(&.to_i)

  rem = lines.skip 1

  boards = rem.each.each_slice 5

  boards = boards.map { |board| Board.new(board.map(&.split(' ', remove_empty: true).map { |n| Square.new(n.to_i) }).to_a) }.to_a

  score = 0

  numbers.each do |number|
    boards = boards.each.select { |b| !b.@completed }.to_a

    break if boards.size == 0

    boards.each do |board|
      if board.mark_square number
        score = board.unmarked_total * number
      end
    end
  end

  score.to_s
}
