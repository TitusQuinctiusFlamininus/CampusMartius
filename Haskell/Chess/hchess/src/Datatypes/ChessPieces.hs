module Datatypes.ChessPieces where

--a specific square on the boar
type Location = (Int, Int)

--the value of the chess piece
type Value = Int

--type of pieces the pawns are (it is used as a phantom, for promotion rules)
data Minor = Minor

--any piece that is not a pawn, is of this type (it is used as a phantom, for promotion rules)
data Major = Major

--colors of the pieces on the chess board
data Color = BLACK | WHITE deriving (Show, Eq)

--fundamental kinds of chess pieces in the game
data PieceType =  KING   | QUEEN  | ROOK  | BISHOP | KNIGHT | PAWN  deriving (Show, Eq)

--a typical chess piece
data Piece a = Piece {   name       :: PieceType,
                         color      :: Color, 
                         worth      :: Value, 
                         location   :: Location
                     }   deriving (Show, Eq)

--create all major pieces (white and black)
setBoard :: [Piece Major]
setBoard             = blackpieces ++ whitepieces
 where rkbTypes      = [ROOK, KNIGHT, BISHOP]
       rkbWorths     = [5, 3, 3]
       nameList      = rkbTypes ++ [QUEEN, KING] ++ reverse rkbTypes
       bColorList    = replicate 8 BLACK
       wColorList    = replicate 8 WHITE
       worthList     = rkbWorths ++ [10, 1000] ++ reverse rkbWorths
       bzipper       = \n c w -> Piece { name  = n, color = c, worth = w, location = (0,8)} 
       wzipper       = \n c w -> Piece { name  = n, color = c, worth = w, location = (0,1)} 
       bNoLocPieces  = zipWith3 bzipper nameList bColorList worthList
       wNoLocPieces  = zipWith3 wzipper nameList wColorList worthList
       blackpieces   = zipWith (\p l -> p {location = (l,8)}) bNoLocPieces [1..8]
       whitepieces   = zipWith (\p l -> p {location = (l,1)}) wNoLocPieces [1..8]
       
--BLACK PAWNS
bPawns :: [Piece Minor]
bPawns = map (\x -> Piece { name  = PAWN, color = BLACK, worth = 1, location = (x,7)} ) [1..8]


--WHITE PAWNS
wPawns :: [Piece Minor]
wPawns = map (\x -> Piece { name  = PAWN, color = WHITE, worth = 1, location = (x,2)} ) [1..8]
