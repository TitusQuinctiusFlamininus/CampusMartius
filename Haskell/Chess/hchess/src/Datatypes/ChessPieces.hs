module Datatypes.ChessPieces where

import Datatypes.ChessTypes 

import Data.List     (zipWith4)

--function to replicate a coordinate 8 times
mult :: a -> [a]
mult = replicate 8 

--function to create the Kings
allKings :: [Piece ZIEL]
allKings = zipWith (\c y -> Piece { name  = KING, color = c, worth = 1000, location = (5,y)} ) [BLACK, WHITE] [8,1]

--function to create all minor pieces, white and black (pawns)       
allMinorPieces :: [Piece MINOR]
allMinorPieces    = pawner BLACK 7 ++ pawner WHITE 2

--function to create pawns of a certain color, placed on a specific row
pawner :: Color -> Row -> [Piece MINOR]
pawner c r = zipWith3 (\c x y -> Piece { name  = PAWN, color = c, worth = 1, location = (x,y)} ) (mult c) [1..8] (mult r)

--function to create all major pieces, white and black (Rooks, Knights, Bishops, Queens, Kings)
allMajorPieces ::    [Piece MAJOR]
allMajorPieces       = blackpieces ++ whitepieces
 where rkbTypes      = [ROOK, KNIGHT, BISHOP]
       rkbWorths     = [5, 3, 3]
       nameList      = rkbTypes  ++ [QUEEN]    ++ reverse rkbTypes
       worthList     = rkbWorths ++ [10, 1000] ++ reverse rkbWorths
       zipper        = \n c w i-> Piece { name  = n, color = c, worth = w, location = (0,i)} 
       bNoLocPieces  = zipWith4 zipper nameList (mult BLACK) worthList (mult 8)
       wNoLocPieces  = zipWith4 zipper nameList (mult WHITE) worthList (mult 1)
       blackpieces   = zipWith (\p l -> p {location = (l,8)}) bNoLocPieces [1..8]
       whitepieces   = zipWith (\p l -> p {location = (l,1)}) wNoLocPieces [1..8]
       
