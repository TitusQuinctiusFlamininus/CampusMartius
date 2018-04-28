{-# LANGUAGE ViewPatterns #-}

module Datatypes.ChessPlay where

import Datatypes.ChessTypes

--function to take any list, duplicate it, negate all values in the duplicate and concatenate to the original list
dupNegateHalf :: (Num a) => [a] -> [a]
dupNegateHalf o = o ++ map negate o
              

--determine unmodified (raw) possible moves of a piece, based on its current position 
mPossibility :: Piece a -> Moves [Location]
mPossibility (name -> KNIGHT) = Moves $ zipWith (\f r -> (f,r)) (dupNegateHalf [2,2,1,1]) (concat . (replicate 2) $ [1,-1,2,-2])