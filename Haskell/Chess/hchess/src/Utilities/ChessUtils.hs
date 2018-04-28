module Utilities.ChessUtils where

import Datatypes.ChessTypes

--function that gives the file for any piece
cFile :: Piece a -> File
cFile = fst . location
                                          

--function that gives the rank for any piece
cRank :: Piece a -> Rank
cRank = snd . location

--function to filter out all all bad ranks (ranks that are outside the board)
outerRanks :: [Location] -> [Location]
outerRanks = filter (\k -> snd k >= 1) . filter (\k -> snd k <= 8)

--function to filter out all all bad files (files that are outside the board)
outerFiles :: [Location] -> [Location]
outerFiles = filter (\k -> fst k >= 1) . filter (\k -> fst k <= 8)
                
--function to obtain all locations for black pieces                 
allColorLocations :: [BoardPiece] -> Color -> [Location]
allColorLocations [] _  = []
allColorLocations (x:xs) c =  if (getColor x == c) then (getLocation x : allColorLocations xs c) else allColorLocations xs c

--function to get the color of any board piece
getColor :: BoardPiece -> Color
getColor (K p)  = color p
getColor (MI p) = color p
getColor (MA p) = color p

--function to get the location of any board piece
getLocation :: BoardPiece -> Location
getLocation (K p)  = location p
getLocation (MI p) = location p
getLocation (MA p) = location p
