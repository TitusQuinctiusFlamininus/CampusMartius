module Hangman where
    
type UGuess = Char

data HangWord    = HangWord { unhang   :: [Char],
                              idx      ::    Int,
                              solution :: [Char]
                            }


guessLetter :: UGuess -> HangWord -> HangWord
guessLetter ' '   h  = h
guessLetter g     h  = case safeRetr (solution h) (idx h) of 
                        Nothing    -> h { idx = (idx h) + 1 }
                        Just g     -> h { unhang = unhang h ++ [g], idx = (idx h) + 1 }
                        
                        
                        
safeRetr :: [a] -> Int -> Maybe a
safeRetr []   _     = Nothing
safeRetr l    n     = if n > length l then Nothing else Just $ l !! n