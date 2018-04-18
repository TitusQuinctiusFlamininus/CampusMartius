module Datatypes.ChessTypes where

--the numeral representing the row a piece is in
type Rank = Int

--the numeral representing the file (column) a piece is in
type File = Int 

--a specific square on the board
type Location = (File, Rank)

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

--typeclass for minor pieces
class Minor a where
    moveBack :: a -> Bool

--making only minor pieces a member 
instance Minor MINOR where
    moveBack MINOR = False

--typeclass for major pieces
class Major a where
    moveAnyDirection :: a -> Bool

--making major pieces a member 
instance Major MAJOR where
    moveAnyDirection MAJOR = True

--making Kings a member 
instance Major ZIEL where
    moveAnyDirection ZIEL = True

 
--a typical chess piece
data Piece a = Piece {   name       :: PieceType,
                         color      :: Color, 
                         worth      :: Value, 
                         location   :: Location
                     }   deriving (Show, Eq)

--the type that we use to gather all chess types together
data Board = K (Piece ZIEL) | MI (Piece MINOR) | MA (Piece MAJOR)

--to show us the board, we are only interested in the pieces
instance Show Board where
    show (K  p)   = show p
    show (MI p)   = show p
    show (MA p)   = show p
  