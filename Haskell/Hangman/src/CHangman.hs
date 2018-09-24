{-# LANGUAGE RecordWildCards #-}

module CHangman where

import Hangman
import Control.Comonad
import qualified Data.HashMap as H   (Map, toList, fromList)



data Chances        = Chances   { ch   :: Int,
                                  uh   ::  [Char],
                                  sol  ::  H.Map [Int] Char
                                } 

data HangStuff a    = HangStuff { g    ::  Char,
                                  u    ::  [Char], 
                                  c    ::  a,
                                  m    ::  H.Map [Int] Char
                                } 

instance Functor HangStuff where
    fmap f HangStuff {c = y}  = HangStuff {c = f y}
    
    
instance Comonad HangStuff where
    extract    (HangStuff {c = y}) = y
    extend z k@(HangStuff {..})    = HangStuff {c = z k}
    
    

guessLetter' :: HangStuff Chances -> Chances
guessLetter' h  = 
    case jury (g h) (m h) of 
          Nothing       ->  Chances {ch   = (ch (c h)) - 1, 
                                     uh   = u h, 
                                     sol  = m h
                                    } 
          Just (n, s')  ->  Chances { ch  = (ch (c h)),
                                      uh  = l ++ ((g h) : (drop 1 r)), 
                                      sol = s'
                                    }
              where (l,r) = splitAt n $ u h
                     