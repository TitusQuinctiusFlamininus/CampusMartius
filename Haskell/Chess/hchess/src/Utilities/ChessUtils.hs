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
(>!<)   c (x:xs)
 | (x `elem` c) == True =      (>!<) c xs
 | otherwise            =  x : (>!<) c xs

--function to obtain the locations of all pieces of the same color, that are on the board (not including captured pieces)           
territory :: [BoardPiece] -> Color -> [Location]
territory [] _  = []
territory (x:xs) c
 | paint x == c  =  locate x : territory xs c
 | otherwise     =  territory xs c

