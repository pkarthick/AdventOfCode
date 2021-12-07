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
  end

  def check_rows
    @rows.any?(&.all?(&.marked))
  end

  def check_cols
    row_count = @rows[0].size
    (0..row_count - 1).any? { |col| @rows.all?(&.[col].marked) }
  end

  def unmarked_total
    @rows.flatten.reduce (0) { |a, sq| !sq.marked ? a + sq.number : a }
  end

  def mark_square(number)
    if square = @rows.flatten.find { |sq| sq.number == number }
      square.mark
      check_rows || check_cols
    else
      false
    end
  end
end

puts Day.new(4, 1).execute { |input|
  lines = input.lines.each.select { |l| !l.empty? }.to_a

  numbers = lines[0]
    .split(',', remove_empty: true)
    .map(&.to_i)

  rem = lines.skip 1

  boards = rem.each.each_slice 5

  boards = boards.map { |board| Board.new(board.map(&.split(' ', remove_empty: true).map { |n| Square.new(n.to_i) }).to_a) }.to_a

  score = numbers.each do |number|
    if completed_board = boards.find(&.mark_square number)
      break completed_board.unmarked_total * number
    end
  end

  score.to_s
}
