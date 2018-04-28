{-# LANGUAGE ViewPatterns #-}

module Datatypes.ChessPlay where

import Datatypes.ChessTypes

--determine unmodified (raw) possible moves of a piece, based on its current position 
mPossibility :: Piece a -> Moves [Location]
mPossibility (name  -> KNIGHT) = Moves $ zipWith (\f r -> (f, r)) [2,2,1,1,-2,-2,-1,-1] [1,-1,2,-2,1,-1,2,-2]