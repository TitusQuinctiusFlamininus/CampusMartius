{-# LANGUAGE ViewPatterns #-}

module Hangman where
    
type UGuess = Char

data HangWord    = HangWord { unhang   :: [Char],
                              idx      ::    Int,
                              solution :: [Char],
                              chances  :: Int
                            } deriving (Show)


guessLetter :: UGuess -> HangWord -> HangWord
guessLetter ' '   h  = h
guessLetter g     h  = case safeRetr (solution h) (idx h) of 
                        Nothing    -> h { idx = (idx h) + 1, chances = (chances h) - 1 }
                        Just g     -> h { unhang = g : unhang h, idx = (idx h) + 1, chances = (chances h) - 1 }
                        
                        
                        
safeRetr :: [a] -> Int -> Maybe a
safeRetr []                   _         = Nothing
safeRetr l    ((>) (length l)  -> True) = Nothing
safeRetr l                    n         = Just $ l !! n 