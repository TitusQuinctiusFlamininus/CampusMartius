module Utilities.ChessUtils where

import Datatypes.ChessTypes
import Datatypes.ChessConstants

--function to replicate some type 8 times
(<->) :: Int -> a -> [a]
(<->) = replicate 

--function to filter out all locations (or cells) that are completely outside the 8x8 board
notOnBoard :: (Location -> RankOrFile) -> [Location] -> [Location]
notOnBoard f = filter (\k -> f k >= lBound) . filter (\k -> f k <= uBound)

--function to check if any elements from one list appear in the second list; and return only those elements that
--do not appear in the second list
--First parameter = the list of all colored location
--Second parameter = the list of all locations that a piece can move to
(>!<) :: [Location] -> [Location] -> [Location]
(>!<)   []   _    = []
(>!<)   _    []   = []
(>!<)   c (x:xs)  = case x `elem` c of 
                        True  ->     (>!<) c xs
                        False -> x : (>!<) c xs

--function to obtain all locations for black pieces                 
colouredLocations :: [BoardPiece] -> Color -> [Location]
colouredLocations [] _  = []
colouredLocations (x:xs) c =  if (paint x == c) then (locate x : colouredLocations xs c) else colouredLocations xs c

