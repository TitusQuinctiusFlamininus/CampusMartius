
module Datatypes.ChessPlay where

import Datatypes.ChessTypes
import Utilities.ChessUtils

--function to take any list, duplicate it, negate all values in the duplicate and concatenate to the original list
dupNegateHalf :: (Num a) => [a] -> [a]
dupNegateHalf o = o ++ map negate o
              

--determine unmodified (raw) possible moves of a piece, based on its current position 
mPossibility :: Piece a -> Moves [Location]
mPossibility p@Piece{name=KNIGHT} = Moves $ zipWith (\f r -> (f,r)) fileList rankList
                                    where fileList = [(fl+2), (fl+2), (fl+1), (fl+1),(fl-2), (fl-2), (fl-1), (fl-1)]
                                          rankList = concat . replicate 2 $ [(rk+1), (rk-1), (rk+2), (rk-2)]
                                          fl = cFile p
                                          rk = cRank p