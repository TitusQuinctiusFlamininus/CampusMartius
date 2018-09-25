{-# LANGUAGE RecordWildCards #-}

module CHangman where

import Hangman
import Control.Comonad
import qualified Data.HashMap as H   (Map, toList, fromList)



data Chances        = Chances   { ch   ::  Int,
                                  uh   ::  [Char],
                                  sol  ::  H.Map [Int] Char
                                } 

data HangStuff a    = HangStuff { g    ::  Char,
                                  c    ::  a
                                } 

instance Functor HangStuff where
    fmap f HangStuff {c = y}  = HangStuff {c = f y}
    
    
instance Comonad HangStuff where
    extract     HangStuff {c = y}  = y
    extend z k@(HangStuff {..})    = HangStuff {c = z k}
    
    

guessLetter' :: HangStuff Chances -> Chances
guessLetter' h  =
    case jury (g h) $ sol zoz of 
          Nothing       ->  Chances {ch   = (ch zoz) - 1, 
                                     uh   = uh zoz, 
                                     sol  = sol zoz
                                    } 
          Just (n, s')  ->  Chances { ch  = ch zoz,
                                      uh  = l ++ ((g h) : (drop 1 r)), 
                                      sol = s'
                                    }
                            where (l,r) = splitAt n $ uh zoz
    where zoz                           = extract h            