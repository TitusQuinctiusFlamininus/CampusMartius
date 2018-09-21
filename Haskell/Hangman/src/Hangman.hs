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


data HangWord    = HangWord { unhang   ::[UGuess],    -- <-- The Guess Word Structure so far constructed by Hangman
                              chances  ::      Int    -- <-- The number of tries left before we get hanged (0 = lights out)
                            } deriving (Show)

guessLetter :: (UGuess, Solution) -> HangWord -> HangWord
guessLetter (' ',s )  h       = h
guessLetter (g  ,s )  h       = 
    case jury g s of 
          Nothing       ->  h { chances = (chances h) - 1 }
          Just (n, s')  ->  h { unhang = update           }
    where update = undefined                   
                        
                        
jury :: UGuess -> Solution -> Maybe (Idx, Solution)
jury  g s    = case (locateGuess g $ H.toList s) of 
                 Nothing -> Nothing
                 Just x  -> case x of 
                             w@(i:[])  -> Just (i, H.delete w s)
                             w@(i:is)  -> Just (i, H.insert is g (H.delete w s))
 
-- | function to find the indices that represent where that character appears in the solution string
locateGuess :: UGuess -> [([Idx], Char)] -> Maybe [Idx]
locateGuess    _   []           = Nothing
locateGuess    g   ((ix,v):xs)  = if g == v then (Just ix) else locateGuess g xs
