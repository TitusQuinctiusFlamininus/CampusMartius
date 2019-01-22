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
                                  c    ::  a,
                                  idx  :: HangStart
                                } 

instance Functor HangStuff where
    fmap f h@HangStuff {c = y}  = HangStuff {c = f y, idx = idx h, g = g h}
    
    
instance Comonad HangStuff where
    extract     HangStuff {c = y}  = y
    extend            z k          = HangStuff {c = z k, idx = idx k, g = g k}

class Advance t where
    up   :: t -> t
    
instance Advance (HangStuff a) where
    up h   = HangStuff {idx = (idx h)+1, g = g h, c = c h}
    
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