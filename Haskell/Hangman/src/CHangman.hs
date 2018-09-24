{-# LANGUAGE RecordWildCards #-}

module CHangman where

import Control.Comonad
import qualified Data.HashMap as H   (Map, toList, fromList)


data Chances = Chances Int

data HangStuff a    = HangStuff { g    ::  Char,
                                  u    ::  [Char], 
                                  c    ::  a,
                                  m    ::  H.Map [Int] Char
                                } deriving (Show, Eq)

instance Functor HangStuff where
    fmap f HangStuff {c = y}  = HangStuff {c = f y}
    
    
instance Comonad HangStuff where
    extract    (HangStuff {c = y}) = y
    extend z k@(HangStuff {..})    = HangStuff {c = z k}