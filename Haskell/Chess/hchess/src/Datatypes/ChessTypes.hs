{-# LANGUAGE MultiParamTypeClasses #-}

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
data BoardPiece = K (Piece ZIEL) | MI (Piece MINOR) | MA (Piece MAJOR) deriving (Eq)

--to show us the board, we are only interested in the pieces
instance Show BoardPiece where
    show (K  p)   = show p
    show (MI p)   = show p
    show (MA p)   = show p
 

--designates the locations possible by any piece, at any one time
data Moves s = Moves s deriving (Show, Eq)
 
--functor instance
--fmap :: (Functor f) => (a -> b) -> fa -> fb
instance Functor Moves where
    fmap f (Moves p) = Moves (f p)

--applicative instance
-- pure :: (Applicative f) => a -> f a
-- <*>  :: (Applicative f) => f(a -> b) -> f a -> f b
instance Applicative Moves where
    pure a  = Moves a
    Moves f <*> Moves s = Moves (f s)
    
--monad instance
-- return :: (Monad m) => a -> m a
-- (>>=)  :: m a -> (a -> m b) -> m b
instance Monad Moves where
    return a = Moves a
    Moves s >>= f = f s
    
--typeclass embodying the ability of a piece to translocate from one square to another
--either to capture or simply to move
--for move:  --a piece to move and its intended location
--for capture:  -- a killer, a victim, a list of all victims captured so far
class Movable p where
    move    :: p -> Location -> p
    capture :: p -> p -> [p] -> (p, [p])

--lets make all our pieces movable and the ability to capture other pieces
instance Movable (Piece a) where
    move p l        = p  { location = l } 
    capture k v l   = (k {location = location v}, (v:l))

--typeclass representing the promotion of minor pieces (i.e pawns). Major pieces are any pieces that are NOT pawns (and not the King)
class Promotable p t where
    promote :: p -> t -> t

--promoting a minor piece to major piece
instance (Minor a, Major b) => Promotable (Piece a) (Piece b) where
    promote p r = Piece {name=name r, color=color p, worth=worth r, location=(fst $ location p, if (color p == BLACK) then 1 else 8)}
    