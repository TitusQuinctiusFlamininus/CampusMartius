module CHangman where

import Control.Comonad
import qualified Data.HashMap as H   (Map, toList, fromList)


data HangStuff a    = HangStuff { u    ::  [a], 
                                  c    ::  Int,
                                  m    ::  H.Map [Int] a
                                } deriving (Show, Eq)

instance Functor HangStuff where
    fmap f HangStuff {u = x, c = y, m = z}  = HangStuff { u = fmap f x, 
                                                          c = y, 
                                                          m = H.fromList $ fmap (\(k,v) -> (k, (f v))) $ H.toList z
                                                        }
    
    
--instance Comonad HangStuff where   