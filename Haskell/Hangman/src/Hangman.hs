{-# LANGUAGE ViewPatterns #-}

module Hangman where

import Data.Foldable
import Data.Hashable   (Hashable)
import qualified Data.HashMap as H   (Map, toList, delete, insert)

-- | Represents an index position of a single character in a string
type Idx    = Int 
-- | Represents a guess by a player, which is always a single character at a time
type UGuess = Char

-- | If any word (the solution, or word the user is supposed to guess and get right) is represented as a string of characters, then the keys represent the indices of the occurrences of some character that composes the word. As the player guesses, the type's internal structure will change
type Solution    = H.Map [Idx] Char                    

-- | Represents the progress of the player making guesses as the game progresses
data HangWord    = HangWord { uhang    ::  [UGuess],  -- <-- The Guess Word Structure so far constructed by Hangman
                              chances  ::  Int        -- <-- The number of tries left before we get hanged (0 = lights out)
                            } deriving (Show)

-- | Formal function to process the guessed letter. UGuess represents the player's guess, 
--   the Solution represents a map of the dictionary word with character indices
guessLetter :: (UGuess, Solution) -> HangWord -> HangWord
guessLetter (' ',s )  h       = h
guessLetter (g  ,s )  h       = 
    case jury g s of 
          Nothing       ->  h { chances = (chances h) - 1 }
          Just (n, s')  ->  h { uhang   = l ++ (g : (drop 1 r))         }
                  where (l,r) = splitAt n $ uhang h
                    

-- | Function that will adjust the solution to reflect whether guess was right or wrong. If guess is correct, then 
--   the entry (of that character) will be deleted from the map if it occurs only once, otherwise we will just 
--   remove the foremost index in a list the represents where the character appears multiple times in the word solution
jury :: UGuess -> Solution -> Maybe (Idx, Solution)
jury  g s    = 
    case (locateGuess g $ H.toList s) of 
          Nothing -> Nothing
          Just x  -> case x of 
                      w@(i:[])  -> Just (i, H.delete w s)
                      w@(i:is)  -> Just (i, H.insert is g (H.delete w s))
 
-- | function to find the indices that represent where that character appears in the solution string. 
--   If it cannot be found, it means the guess was wrong and we return Nothing, otherwise we return the 
--   key in the solutionMap 
locateGuess :: UGuess -> [([Idx], Char)] -> Maybe [Idx]
locateGuess  _   []           = Nothing
locateGuess  g   ((ix,v):xs)  = if g == v then (Just ix) else locateGuess g xs
