module Utilities.ChessUtils where

import Datatypes.ChessTypes
import Datatypes.ChessConstants

-- | Replicate some type a number of times
(<->) :: Int -> a -> [a]
(<->) = replicate 

--Filter out all locations (or cells) that are completely outside the 8x8 board
notOnBoard :: (Location -> RankOrFile) -> [Location] -> [Location]
notOnBoard f = filter (\k -> f k >= lBound) . filter (\k -> f k <= uBound)

-- | Check if any elements from one list appear in the second list; and return only those elements that
--   do not appear in the second list
--   First parameter = the list of all colored location
--   Second parameter = the list of all locations that a piece can move to
(>!<) :: [Location] -> [Location] -> [Location]
(>!<)   []   _    = []
(>!<)   _    []   = []
(>!<)   c (x:xs)
 | (x `elem` c) == True =      (>!<) c xs
 | otherwise            =  x : (>!<) c xs

-- | Obtain the locations of all pieces of the same color, that are on the board (not including captured pieces)           
territory :: [BoardPiece] -> Color -> [Location]
territory [] _  = []
territory (x:xs) c
 | paint x == c  =  locate x : territory xs c
 | otherwise     =  territory xs c

-- | Zip two lists where:
--   First list is a list of functions (each representing a binary addition operation with one value already bound )
--   Second list is a list of spans
(|+|) :: (RankOrFile -> RankOrFile) -> [RankOrFile]
(|+|) h = zipWith ($) ((<->) uBound h) boardSpan

-- | Zip two lists where:
--   First list is a list of values (representing either files or ranks)
--   Second list is a list of spans
(|-|) :: RankOrFile -> [RankOrFile]
(|-|) g = zipWith (-) ((<->) uBound g) boardSpan 