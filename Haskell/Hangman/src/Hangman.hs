{-# LANGUAGE ViewPatterns #-}

module Hangman where

import Data.Foldable
import Data.Hashable   (Hashable)
import Data.HashMap 

-- | Represents an index position of a single character in a string
type Idx    = Int 
-- | Represents a guess by a player, which is always a single character at a time
type UGuess = Char

-- | If any word (the solution, or word the user is supposed to guess and get right) is represented as a string of characters, then the keys represent the indices of the occurrences of some character that composes the word. As the player guesses, the type's internal structure will change
type Solution    = Map [Idx] Char                    


data HangWord    = HangWord { unhang   ::[UGuess],    -- <-- The Guess Word Structure so far constructed by Hangman
                              chances  ::      Int    -- <-- The number of tries left before we get hanged (0 = lights out)
                            } deriving (Show)

guessLetter :: (UGuess, Solution) -> HangWord -> ([UGuess], HangWord)
guessLetter (' ',s )  h       = (gs,h)
guessLetter (g  ,s )  h       = 
    case safeRetr (solution h) (idx h) of 
          Nothing    -> (gs,                          h { chances = (chances h) - 1 }  )
          Just g     -> (filterFirstLetterOccurrence, h { unhang = update}             )
    where update = findCharAtIndexAndReplace                   
                        
                        
safeRetr :: [a] -> Idx -> Maybe a
safeRetr []                   _         = Nothing
safeRetr l    ((>) (length l)  -> True) = Nothing
safeRetr l                    n         = Just $ l !! n 