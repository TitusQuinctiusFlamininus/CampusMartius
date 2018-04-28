
module Datatypes.ChessPlay where

import Datatypes.ChessTypes
import Utilities.ChessUtils

--determine unmodified (raw) possible moves of a piece, based on its current position 
mPossibility :: Piece a -> Moves [Location]
mPossibility p@Piece{name=KNIGHT} = return $ zipWith (\f r -> (f,r)) fileList rankList
                                    where fileList = [(fl+2), (fl+2), (fl+1), (fl+1), (fl-2), (fl-2), (fl-1), (fl-1)]
                                          rankList = concat . replicate 2 $  [(rk+1), (rk-1), (rk+2), (rk-2)]
                                          fl = cFile p
                                          rk = cRank p
 
--filter out all locations outside the board, given as locations in the list 
filterNoBoard :: [Location] -> Moves [Location]
filterNoBoard l = return $ outerRanks . outerFiles $ l 