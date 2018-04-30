{-# LANGUAGE ViewPatterns #-}

module Datatypes.ChessPlay where

import Datatypes.ChessTypes
import Datatypes.ChessConstants
import Utilities.ChessUtils

--RAW POSSIBILITIES ACROSS ALL CELLS, in an 8x8 SPACE, regardless of whether the cells are on the board or not
--determine unmodified (raw) possible moves of a piece, based on its current position
(<-?->) :: Location -> PieceType -> Moves [Location]
(<-?->) (f,r) t
 | t == KNIGHT = return $ poss knFiles knRanks
 | t == BISHOP = return $ poss bFiles bRanks
 | t == ROOK   = return $ poss rFiles rRanks
 | otherwise   = return [] 
                 where poss    = zipWith locZipper
                       knFiles = (zipWith ($) ((<->) 2 (+2) ++ (<->) 2 (+1)) $ (<->) 4 $ f) ++ (<->) 2 (f-2) ++ (<->) 2 (f-1)
                       knRanks = concat . (<->) 2 $ [(r+1), (r-1), (r+2), (r-2)]
                       bFiles  = concat . (<->) 2 $ ((|+|) (+f)) ++ ((|-|) f)
                       bRanks  = ((|+|) (+r)) ++ ((|-|) r) ++ ((|-|) r) ++ ((|+|) (+r)) 
                       rFiles  = ((|+|) (+f)) ++ ((|-|) f) ++ ((concat . (<->) 2) $ ((<->) uBound f))
                       rRanks  = ((concat . (<->) 2) $ ((<->) uBound r)) ++ ((|+|) (+r)) ++ ((|-|) r)
                                                           

--FILTER OUT ALL CELLS THAT ARE NOT ON THE BOARD
--filter out all locations outside the board, given as locations in the list 
(.<->.) :: [Location] -> Moves [Location]
(.<->.) l = return $ notOnBoard fst . notOnBoard snd $ l 

--FILTER OUT ALL CELLS ALREADY OCCUPIED BY YOUR (BLACK OR WHITE) OWN PIECES
--function to filter out all locations that have already been occupied one's own color pieces 
--for example, if black wants to move, then he can only do so to a sqaure not occupied by his own pieces
--First parameter  = the list of all colored locations
--Second parameter = the list of locations from the moves monad
(<-!->) :: [Location] -> [Location] -> Moves [Location]
(<-!->) c l = return $ (>!<) c l
