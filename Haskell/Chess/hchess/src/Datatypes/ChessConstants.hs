module Datatypes.ChessConstants where

import Datatypes.ChessTypes 


rkb     = [ROOK, KNIGHT, BISHOP] :: [PieceType]

rkbWorths    = [5, 3, 3]

nameList     = rkb  ++   [QUEEN]  ++ reverse rkb  :: [PieceType]

worthList    = rkbWorths ++     [10]   ++ reverse rkbWorths :: [Value]

zipper       = \n c w file rank -> Piece { name  = n, color = c, worth = w, location = (file,rank)} 

nonKingFiles = [1..4]++[6..8] :: [File]

allFiles     = [1..8] :: [File]

bPawnsRank   = 7 :: Rank

wPawnsRank   = 2 :: Rank
