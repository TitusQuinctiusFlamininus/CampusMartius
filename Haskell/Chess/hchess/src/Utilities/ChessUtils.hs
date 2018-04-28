module Utilities.ChessUtils where

import Datatypes.ChessTypes

--function that gives the file for any piece
cFile :: Piece a -> File
cFile = fst . location
                                          

--function that gives the rank for any piece
cRank :: Piece a -> Rank
cRank = snd . location
                           