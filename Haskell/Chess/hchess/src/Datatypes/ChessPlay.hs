{-# LANGUAGE ViewPatterns #-}

module Datatypes.ChessPlay where

import Datatypes.ChessTypes
import Datatypes.ChessConstants
import Utilities.ChessUtils

-- | Raw possibilities across all cells, in an 8x8 space, regardless of whether the cells are on the board or not.
--   It determines unmodified possible moves of a piece, based on its current position. Some positions resulting may 
--   not even be on the board
(<-?->) :: Location -> PieceType -> PossibleMoves [Location]
(<-?->) (f,r) t
 | t == KNIGHT = return $ poss knFiles knRanks
 | t == KING   = return $ poss kFiles kRanks
 | t == QUEEN  = return $ bMoves ++ rMoves
 | t == BISHOP = return bMoves
 | t == ROOK   = return rMoves
 | otherwise   = return [] 
                 where poss    = zipWith locZipper
                       knFiles = (zipWith ($) ((<->) 2 (+2) ++ (<->) 2 (+1)) $ (<->) 4 $ f) ++ (<->) 2 (f-2) ++ (<->) 2 (f-1)
                       knRanks = concat . (<->) 2 $ [(r+1), (r-1), (r+2), (r-2)]
                       bFiles  = concat . (<->) 2 $ ((|+|) (+f)) ++ ((|-|) f)
                       bRanks  = ((|+|) (+r)) ++ ((|-|) r) ++ ((|-|) r) ++ ((|+|) (+r))
                       rFiles  = ((|+|) (+f)) ++ ((|-|) f) ++ ((concat . (<->) 2) $ ((<->) uBound f))
                       rRanks  = ((concat . (<->) 2) $ ((<->) uBound r)) ++ ((|+|) (+r)) ++ ((|-|) r)
                       kFiles  = [f, f+1, f+1, f+1, f, f-1, f-1, f-1]
                       kRanks  = [r+1, r+1, r, r-1, r-1, r-1, r, r+1]
                       bMoves  = poss bFiles bRanks
                       rMoves  = poss rFiles rRanks
                                                           

-- | Filter out all locations outside the board, given as locations in the list 
(.<->.) :: [Location] -> PossibleMoves [Location]
(.<->.) l = return $ notOnBoard fst . notOnBoard snd $ l 

-- | Function to filter out all locations that have already been occupied one's own color pieces. 
--   For example, if black wants to move, then he can only do so to a sqaure not occupied by his own pieces.
--   First parameter  = the list of all colored locations
--   Second parameter = the list of locations from the moves monad
(<-!->) :: [Location] -> [Location] -> PossibleMoves [Location]
(<-!->) c l = return $ (>!<) c l
