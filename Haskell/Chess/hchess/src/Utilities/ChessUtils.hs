module Utilities.ChessUtils where

import Datatypes.ChessTypes

--function that gives the file for any piece
cFile :: Piece a -> File
cFile = fst . location
                                          
--function that gives the rank for any piece
cRank :: Piece a -> Rank
cRank = snd . location

filterOuterBoard :: (Location -> Int) -> [Location] -> [Location]
filterOuterBoard f = filter (\k -> f k >= lBound) . filter (\k -> f k <= uBound)

--function to obtain all locations for black pieces                 
colouredLocations :: [BoardPiece] -> Color -> [Location]
colouredLocations [] _  = []
colouredLocations (x:xs) c =  if (paint x == c) then (locate x : colouredLocations xs c) else colouredLocations xs c
