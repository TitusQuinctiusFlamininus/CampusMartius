module Utilities.ChessUtils where

import Datatypes.ChessTypes
import Datatypes.ChessConstants

--function to replicate some type 8 times
mult :: Int -> a -> [a]
mult = replicate 

filterOuterBoard :: (Location -> RankOrFile) -> [Location] -> [Location]
filterOuterBoard f = filter (\k -> f k >= lBound) . filter (\k -> f k <= uBound)

--function to obtain all locations for black pieces                 
colouredLocations :: [BoardPiece] -> Color -> [Location]
colouredLocations [] _  = []
colouredLocations (x:xs) c =  if (paint x == c) then (locate x : colouredLocations xs c) else colouredLocations xs c

