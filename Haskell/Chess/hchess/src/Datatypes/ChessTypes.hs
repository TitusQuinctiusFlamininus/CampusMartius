module Datatypes.ChessTypes where

--a specific square on the boar
type Location = (Int, Int)

--the numeral representing the row a piece is in
type Rank = Int

--the value of the chess piece
type Value = Int

--type of pieces the pawns are (it is used as a phantom, for promotion rules)
data MINOR = MINOR

--any piece that is not a pawn, except the king, is of this type (it is used as a phantom, for promotion rules)
data MAJOR = MAJOR

--the type describing the piece (king), which is the goal of chess
data ZIEL  = ZIEL

--colors of the pieces on the chess board
data Color = BLACK | WHITE deriving (Show, Eq)

--fundamental kinds of chess pieces in the game
data PieceType =  KING  | QUEEN  | ROOK  | BISHOP | KNIGHT | PAWN  deriving (Show, Eq)

class Minor a where
    moveBack :: a -> Bool
 
instance Minor MINOR where
    moveBack MINOR = False

class Major a where
    moveAnyDirection :: a -> Bool

instance Major MAJOR where
    moveAnyDirection MAJOR = True

--a typical chess piece
data Piece a = Piece {   name       :: PieceType,
                         color      :: Color, 
                         worth      :: Value, 
                         location   :: Location
                     }   deriving (Show, Eq)