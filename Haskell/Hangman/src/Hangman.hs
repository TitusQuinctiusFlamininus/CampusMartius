{-# LANGUAGE ViewPatterns #-}

module Hangman where

import Data.Foldable
import qualified Data.Sequence as S

type UGuess = Char

data HangWord    = HangWord { unhang   ::[UGuess],
                              idx      ::      Int,
                              solution :: [UGuess],
                              chances  ::      Int
                            } deriving (Show)

guessLetter :: (UGuess, [UGuess]) -> HangWord -> ([UGuess], HangWord)
guessLetter (' ',gs )  h       = (gs,h)
guessLetter (g  ,gs )  h       = 
    case safeRetr (solution h) (idx h) of 
          Nothing    -> (gs,                 h { chances = (chances h) - 1 }              )
          Just g     -> (filterFirstLetterOccurrence, h { unhang = update, idx = (idx h) + 1})
    where update = findCharAtIndexAndReplace                   
                        
                        
safeRetr :: [a] -> Int -> Maybe a
safeRetr []                   _         = Nothing
safeRetr l    ((>) (length l)  -> True) = Nothing
safeRetr l                    n         = Just $ l !! n 