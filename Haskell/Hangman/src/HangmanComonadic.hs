module HangmanComonadic where

import Hangman 

-- extract :: w a -> a
-- extend  :: (w a -> b) -> w a -> w b


data CHangword a = CHangword ((UGuess, Solution) -> HangWord) HangWord 