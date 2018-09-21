{-# LANGUAGE ViewPatterns #-}

module Hangman where
    
type UGuess = Char

data HangWord    = HangWord { unhang   :: [UGuess],
                              idx      ::    Int,
                              solution :: [UGuess],
                              chances  ::    Int
                            } deriving (Show)

-- | What the player has guessed so far
type PartialGuess = [Char]

guessLetter :: (UGuess, [UGuess]) -> HangWord -> ([UGuess], HangWord)
guessLetter (' ',gs )  h       = (gs,h)
guessLetter (g  ,gs )  h       = 
    case safeRetr (solution h) (idx h) of 
          Nothing    -> (gs,                 h { chances = (chances h) - 1 }              )
          Just g     -> ((filter (/= g) gs), h { unhang = g : unhang h, idx = (idx h) + 1})
                        
                        
                        
safeRetr :: [a] -> Int -> Maybe a
safeRetr []                   _         = Nothing
safeRetr l    ((>) (length l)  -> True) = Nothing
safeRetr l                    n         = Just $ l !! n 