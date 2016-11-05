## Solution template for Guess The Word practice problem (section 7)

require_relative './section-7-provided'

class ExtendedGuessTheWordGame < GuessTheWordGame
  ## YOUR CODE HERE
  def is_valid_secrect_word? word
    super(word) && /[^[[:punct:]][[:space:]]-]/.match(word) == nil 
  end
end

class ExtendedSecretWord < SecretWord
  ## YOUR CODE HERE
  def initialize word
    self.word = word
    self.pattern = word.gsub(/[a-zA-Z]/,"-")
  end

  def valid_guess? guess
    return false if !super(guess) || !/[a-zA-Z]/.match(guess)
    if !@pre_guesses
    then
      @pre_guesses = [guess.downcase]
      true
    else
      has_guessed = @pre_guesses.index(guess.downcase)
      @pre_guesses += [guess.downcase] if !has_guessed
      has_guessed == nil
    end
  end
  
  def guess_letter! letter
    find_up = super(letter.upcase)
    find_down = super(letter.downcase)
    find_up || find_down
  end
end

## Change to `false` to run the original game
if true
  ExtendedGuessTheWordGame.new(ExtendedSecretWord).play
else
  GuessTheWordGame.new(SecretWord).play
end
