module HangmanComonadic where

import Hangman 

-- extract :: w a -> a
-- extend  :: (w a -> b) -> w a -> w b

-- | comonadic type
data CHangword a = CWord (UGuess, Solution) HangWord 

