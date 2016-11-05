# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = Piece::All_Pieces +
                  [[[[-2, 0], [-1, 0], [0, 0], [1, 0], [2, 0]], # long long
                   [[0, -2], [0, -1], [0, 0], [0, 1], [0, 2]]],
                   Piece.rotations([[0, 0], [1, 0], [0, -1]]), # small L
                   [[[0, 0], [1, 0], [0, 1], [1, 1], [2, 1]],  # square with tail
                   [[0, 0], [1, 0], [0, 1], [1, 1], [0, 2]],
                   [[0, 0], [1, 0], [0, 1], [1, 1], [-1, 0]],
                   [[0, 0], [1, 0], [0, 1], [1, 1], [1, -1]]]]                                 
                  
  # your enhancements here
  
  # class method to choose the next piece
  def self.next_piece (board)
    Piece.new(All_My_Pieces.sample, board)
  end
  def self.next_cheat_piece (board)
    Piece.new([[[0, 0]]], board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    super(game)
    @current_block = MyPiece.next_piece(self)
  end

  # gets the next piece
  def next_piece
    super
    if @has_cheated
      @current_block = MyPiece.next_cheat_piece(self)
      @has_cheated = false
    else
      @current_block = MyPiece.next_piece(self)
    end
  end
  
  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size-1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
  
  def cheat
    if !@has_cheated && @score >= 100
      @score -= 100
      @has_cheated = true
    end
  end
end

class MyTetris < Tetris
  # your enhancements here

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
  def buttons
    super
    @root.bind('u', proc {@board.rotate_clockwise; @board.rotate_clockwise})
    @root.bind('c', proc {@board.cheat})
  end
end


