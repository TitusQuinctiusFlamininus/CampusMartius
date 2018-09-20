module Hangman where
    
type UGuess = Char

data HangWord    = HangWord { unhang   :: [Char],
                              cpos     ::    Int,
                              solution :: [Char]
                            }


guessLetter :: UGuess -> HangWord -> HangWord
guessLetter ' '   h  = h
guessLetter g     h  = undefined
