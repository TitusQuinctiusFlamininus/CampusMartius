module Datatypes.ChessPieces where

--a specific square on the boar
type Location = (Int, Int)

--the value of the chess piece
type Value = Int

--colors of the pieces on the chess board
data Color = BLACK | WHITE deriving (Show, Eq)

--fundamental kinds of chess pieces in the game
data PieceType =  KING   | QUEEN  | ROOK  | BISHOP | KNIGHT | PAWN  deriving (Show, Eq)

--a typical chess piece
data Piece = Piece {   name       :: PieceType,
                       color      :: Color, 
                       worth      :: Value, 
                       location   :: Location
                   }
                      
                      
{-BLACK CHESS PIECES-}

--BLACK KING
bKing :: Piece
bKing =  Piece { name  = KING, color = BLACK, worth = 1000, location = (5,8)} 

--BLACK QUEEN
bQueen :: Piece
bQueen =  Piece { name  = QUEEN, color = BLACK, worth = 10, location = (4,8)} 

--BLACK KING BISHOP
bkBishop :: Piece
bkBishop =  Piece { name  = BISHOP, color = BLACK, worth = 3, location = (6,8)} 

--BLACK QUEEN  BISHOP
bqBishop :: Piece
bqBishop =  Piece { name  = BISHOP, color = BLACK, worth = 3, location = (3,8)} 

--BLACK KING KNIGHT
bkKnight :: Piece
bkKnight =  Piece { name  = KNIGHT, color = BLACK, worth = 3, location = (7,8)} 

--BLACK QUEEN KNIGHT
bqKnight :: Piece
bqKnight =  Piece { name  = KNIGHT, color = BLACK, worth = 3, location = (2,8)} 

--BLACK KING ROOK
bkRook :: Piece
bkRook =  Piece { name  = ROOK, color = BLACK, worth = 5, location = (8,8)} 

--BLACK QUEEN ROOK
bqRook :: Piece
bqRook =  Piece { name  = ROOK, color = BLACK, worth = 5, location = (1,8)} 

--BLACK PAWNS
bPawns :: [Piece]
bPawns = map (\x -> Piece { name  = PAWN, color = BLACK, worth = 1, location = (x,7)} ) [1..8]

{-WHITE CHESS PIECES-}

--WHITE KING
wKing :: Piece
wKing =  Piece { name  = KING, color = WHITE, worth = 1000, location = (5,1)} 

--WHITE QUEEN
wQueen :: Piece
wQueen =  Piece { name  = QUEEN, color = WHITE, worth = 10, location = (4,1)} 

--WHITE KING BISHOP
wkBishop :: Piece
wkBishop =  Piece { name  = BISHOP, color = WHITE, worth = 3, location = (6,1)} 

--WHITE QUEEN  BISHOP
wqBishop :: Piece
wqBishop =  Piece { name  = BISHOP, color = WHITE, worth = 3, location = (3,1)} 

--WHITE KING KNIGHT
wkKnight :: Piece
wkKnight =  Piece { name  = KNIGHT, color = WHITE, worth = 3, location = (7,1)} 

--WHITE QUEEN KNIGHT
wqKnight :: Piece
wqKnight =  Piece { name  = KNIGHT, color = WHITE, worth = 3, location = (2,1)} 

--WHITE KING ROOK
wkRook :: Piece
wkRook =  Piece { name  = ROOK, color = WHITE, worth = 5, location = (8,1)} 

--WHITE QUEEN ROOK
wqRook :: Piece
wqRook =  Piece { name  = ROOK, color = WHITE, worth = 5, location = (1,1)} 

--WHITE PAWNS
wPawns :: [Piece]
wPawns = map (\x -> Piece { name  = PAWN, color = WHITE, worth = 1, location = (x,2)} ) [1..8]
