module Hangman where
    
type UGuess = Char

data HangWord    = HangWord { unhang   :: [Char],
                              idx      ::    Int,
                              solution :: [Char]
                            }


guessLetter :: UGuess -> HangWord -> HangWord
guessLetter ' '   h  = h
guessLetter g     h  = case solution h !! idx h of 
                        g    -> h { unhang = unhang h ++ [g], idx = (idx h) + 1 }
                        _    -> h { idx = (idx h) + 1 }
