module Datatypes.ChessPieces where

import Data.List     (zipWith4)

--a specific square on the boar
type Location = (Int, Int)

--the numeral representing the row a piece is in
type Row = Int

--the value of the chess piece
type Value = Int

--type of pieces the pawns are (it is used as a phantom, for promotion rules)
data Minor = Minor

--any piece that is not a pawn, is of this type (it is used as a phantom, for promotion rules)
data Major = Major

--colors of the pieces on the chess board
data Color = BLACK | WHITE deriving (Show, Eq)

--fundamental kinds of chess pieces in the game
data PieceType =  KING  | QUEEN  | ROOK  | BISHOP | KNIGHT | PAWN  deriving (Show, Eq)

class MINOR a where
    moveBack :: a -> Bool
 
instance MINOR Minor where
    moveBack Minor = False

class MAJOR a where
    moveAnyDirection :: a -> Bool

instance MAJOR Major where
    moveAnyDirection Major = True

--a typical chess piece
data Piece a = Piece {   name       :: PieceType,
                         color      :: Color, 
                         worth      :: Value, 
                         location   :: Location
                     }   deriving (Show, Eq)

--function to create all major pieces, white and black (Rooks, Knights, Bishops, Queens, Kings)
allMajorPieces :: [Piece Major]
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
allMinorPieces :: [Piece Minor]
allMinorPieces    = pawner 7 BLACK ++ pawner 2 WHITE
 where pawner r c = map (\x -> Piece { name  = PAWN, color = c, worth = 1, location = (x,r)} ) [1..8]
