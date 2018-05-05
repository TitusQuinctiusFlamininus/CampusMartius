{-# LANGUAGE MultiParamTypeClasses #-}

module Datatypes.ChessTypes where

import Datatypes.ChessConstants

{--
*****************************
*****************************
******HCHESS CONSTANTS*******
*****************************
*****************************
--}

rkb     = [ROOK, KNIGHT, BISHOP] :: [PieceType]

majNames     = rkb  ++   [QUEEN]  ++ reverse rkb  :: [PieceType]

majWorths    = rkbWorths ++     [10]   ++ reverse rkbWorths :: [Value]

kingWorth    = 1000      :: Value

kingFile     = 5         :: File

pawnWorth    = 1         :: Value

zipper       = \n c w file rank -> Piece { name  = n, color = c, worth = w, location = (file,rank)} 

locZipper    = \f r -> (f,r)

nonKingFiles = [1..4]++[6..8] :: [File]

allFiles     = [1..8] :: [File]

bPawnsRank   = 7 :: Rank

wPawnsRank   = 2 :: Rank


{--
*****************************
*****************************
***HCHESS TYPE SYNONYMS******
*****************************
*****************************
--}

--an integer that describes a part of a location tuple, either rank or file 
type RankOrFile = Int

--the numeral representing the row a piece is in
type Rank = Int

--the numeral representing the file (column) a piece is in
type File = Int 

--a specific square on the board
type Location = (File, Rank)

--the value of the chess piece
type Value = Int

{--
*****************************
*****************************
******HCHESS DATA TYPES******
*****************************
*****************************
--}

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

--a typical chess piece
data Piece a = Piece {   name       :: PieceType,
                         color      :: Color, 
                         worth      :: Value, 
                         location   :: Location
                     }   deriving (Show, Eq)

--the type that we use to gather all chess types together
data BoardPiece = K (Piece ZIEL) | MI (Piece MINOR) | MA (Piece MAJOR) deriving (Eq)

--designates the locations possible by any piece, at any one time
data PossibleMoves s = PossibleMoves s deriving (Show, Eq)

--the type that models the game moves our program can make based on the opponents responses
--the tree will be formed when we absorb pgn chess game files that have already been played
--the program will go down a path in the tree depending upon whether we can find a match 
--with the move just made by the opponent
data GameTree m c d s = Move m c d s [GameTree m c d s] deriving (Show, Eq)

{--
*****************************
*****************************
****HCHESS TYPE CLASSES******
*****************************
*****************************
--}

--typeclass for minor pieces
class Minor a where
    moveBack :: a -> Bool

--typeclass for major pieces
class Major a where
    moveAnyDirection :: a -> Bool

--typeclass to help us get behaviour out of the higher level board piece types
class Boarder a where
    paint  :: a -> Color
    locate :: a -> Location

--typeclass embodying the ability of a piece to migrate from one square to another
--either to capture or simply to move
--for move:  --a piece to move and its intended location
--for capture:  -- a killer, a victim, a list of all victims captured so far
class Movable p where
    move    :: p -> Location -> p
    capture :: p -> p -> [p] -> (p, [p])

--typeclass representing the promotion of minor pieces (i.e pawns). Major pieces are any pieces that are NOT pawns (and not the King)
class Promotable p t where
    promote :: p -> t -> t


{--
*****************************
*****************************
**HCHESS TYPECLASS INSTANCES*
*****************************
*****************************
--}


--making only minor pieces a member 
instance Minor MINOR where
    moveBack MINOR = False


--making major pieces a member 
instance Major MAJOR where
    moveAnyDirection MAJOR = True

--making Kings a member 
instance Major ZIEL where
    moveAnyDirection ZIEL = True

--instances of board pieces
instance Boarder BoardPiece where
    paint  (K p)   = color p
    paint  (MI p)  = color p
    paint  (MA p)  = color p
    locate (K p)   = location p
    locate (MI p)  = location p
    locate (MA p)  = location p

--to show us the board, we are only interested in the pieces
instance Show BoardPiece where
    show (K  p)   = show p
    show (MI p)   = show p
    show (MA p)   = show p


--functor instance
--fmap :: (Functor f) => (a -> b) -> fa -> fb
instance Functor PossibleMoves where
    fmap f (PossibleMoves p) = PossibleMoves (f p)

--applicative instance
-- pure :: (Applicative f) => a -> f a
-- <*>  :: (Applicative f) => f(a -> b) -> f a -> f b
instance Applicative PossibleMoves where
    pure = PossibleMoves
    PossibleMoves f <*> PossibleMoves s = PossibleMoves (f s)
    
--monad instance
-- return :: (Monad m) => a -> m a
-- (>>=)  :: m a -> (a -> m b) -> m b
instance Monad PossibleMoves where
    return = PossibleMoves
    PossibleMoves s >>= f = f s
    

--lets make all our pieces movable and the ability to capture other pieces
instance Movable (Piece a) where
    move p l        = p  { location = l } 
    capture k v l   = (k {location = location v}, (v:l))


--promoting a minor piece to major piece
instance (Minor a, Major b) => Promotable (Piece a) (Piece b) where
    promote p r = Piece { name     =  name r, 
                          color    =  color p, 
                          worth    =  worth r, 
                          location =  (fst $ location p, if (color p == BLACK) then uBound else lBound)
                         }
    