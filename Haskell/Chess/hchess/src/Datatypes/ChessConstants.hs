module Datatypes.ChessConstants where

import Datatypes.ChessTypes 


rkbTypes   = [ROOK, KNIGHT, BISHOP]

rkbWorths  = [5, 3, 3]

nameList   = rkbTypes  ++   [QUEEN]  ++ reverse rkbTypes

worthList  = rkbWorths ++     [10]   ++ reverse rkbWorths :: [Value]

zipper     = \n c w col row -> Piece { name  = n, color = c, worth = w, location = (col,row)} 
