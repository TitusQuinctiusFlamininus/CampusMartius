module Utilities.ChessUtils where

import Datatypes.ChessTypes
import Datatypes.ChessConstants

--function to replicate some type 8 times
(<||>) :: Int -> a -> [a]
(<||>) = replicate 

--function to filter out all locations (or cells) that are completely outside the 8x8 board
(\/) :: (Location -> RankOrFile) -> [Location] -> [Location]
(\/) f = filter (\k -> f k >= lBound) . filter (\k -> f k <= uBound)

--function to check if any elements from one list appear in the second list; and return only those elements that
--do not appear in the second list
(!>) :: [Location] -> [Location] -> [Location]
(!>)   []   _  = []
(!>)   _    [] = []
(!>) (x:xs) c  = case x `elem` c of 
                        True  ->     (!>) xs c
                        False -> x : (!>) xs c

--function to obtain all locations for black pieces                 
colouredLocations :: [BoardPiece] -> Color -> [Location]
colouredLocations [] _  = []
colouredLocations (x:xs) c =  if (paint x == c) then (locate x : colouredLocations xs c) else colouredLocations xs c

