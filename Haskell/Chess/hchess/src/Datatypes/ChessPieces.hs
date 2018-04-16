module Datatypes.ChessPieces where

import Datatypes.ChessTypes 

import Data.List     (zipWith4)

--function to create all major pieces, white and black (Rooks, Knights, Bishops, Queens, Kings)
allMajorPieces :: [Piece MAJOR]
allMajorPieces       = blackpieces ++ whitepieces
 where rkbTypes      = [ROOK, KNIGHT, BISHOP]
       rkbWorths     = [5, 3, 3]
       nameList      = rkbTypes ++ [QUEEN, KING] ++ reverse rkbTypes
       bColorList    = replicate 8 BLACK
       wColorList    = replicate 8 WHITE
       worthList     = rkbWorths ++ [10, 1000] ++ reverse rkbWorths
       zipper        = \n c w i-> Piece { name  = n, color = c, worth = w, location = (0,i)} 
       bNoLocPieces  = zipWith4 zipper nameList bColorList worthList (replicate 8 8)
       wNoLocPieces  = zipWith4 zipper nameList wColorList worthList (replicate 8 1)
       blackpieces   = zipWith (\p l -> p {location = (l,8)}) bNoLocPieces [1..8]
       whitepieces   = zipWith (\p l -> p {location = (l,1)}) wNoLocPieces [1..8]
       
--function to create all minor pieces, white and black (pawns)       
allMinorPieces :: [Piece MINOR]
allMinorPieces    = pawner 7 BLACK ++ pawner 2 WHITE
 where pawner r c = map (\x -> Piece { name  = PAWN, color = c, worth = 1, location = (x,r)} ) [1..8]
