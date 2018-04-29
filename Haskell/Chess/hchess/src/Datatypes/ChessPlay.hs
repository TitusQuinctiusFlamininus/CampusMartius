{-# LANGUAGE ViewPatterns #-}

module Datatypes.ChessPlay where

import Datatypes.ChessTypes
import Datatypes.ChessConstants
import Utilities.ChessUtils


--determine unmodified (raw) possible moves of a piece, based on its current position 
mPossibility :: Location -> PieceType -> Moves [Location]
mPossibility (f,r) t
 | t == KNIGHT = return $ poss kFiles kRanks
 | t == BISHOP = return $ poss bFiles bRanks
 | otherwise   = return [] 
                 where poss   = zipWith locZipper 
                       kFiles = (zipWith ($) ((<||>) 2 (+2) ++ (<||>) 2 (+1)) $ (<||>) 4 $ f) ++ (<||>) 2 (f-2) ++ (<||>) 2 (f-1)
                       kRanks = concat . (<||>) 2 $ [(r+1), (r-1), (r+2), (r-2)]
                       bFiles = concat . (<||>) 2 $ zipWith ($) ((<||>) uBound (+f)) boardSpan ++ zipWith ((-)) ((<||>) uBound f) boardSpan
                       bRPos  = zipWith ($)   ((<||>) uBound (+r)) boardSpan 
                       bRNeg  = zipWith ((-)) ((<||>) uBound r)    boardSpan
                       bRanks = bRPos ++ bRNeg ++ bRNeg ++ bRPos -- <- this is NOT a reversed list 
                                                           

--filter out all locations outside the board, given as locations in the list 
filterNoBoard :: [Location] -> Moves [Location]
filterNoBoard l = return $ (\/) fst . (\/) snd $ l 

--function to filter out all locations that have already been occupied one's own color pieces 
--for example, if black wants to move, then he can only do so to a sqaure not occupied by his own pieces
--first parameter = from the Moves monad
--second parameter = the list of all colored locations
filterOwnOccupied :: [Location] -> [Location] -> Moves [Location]
filterOwnOccupied l c = return $ (!>) l c

