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

-- | Rook, Knight and Bishop Piece types are a list
rkb     = [ROOK, KNIGHT, BISHOP] :: [PieceType]

-- | What the back row of a setup chessboard looks like, omitting the King (from left to right, or vice-versa)
majNames     = rkb  ++   [QUEEN]  ++ reverse rkb  :: [PieceType]

-- | What the back row worths of a setup chessboard looks like, omitting the King (from left to right, or vice-verse)
majWorths    = rkbWorths ++     [10]   ++ reverse rkbWorths :: [Value]

-- | The King's worth
kingWorth    = 1000      :: Value

-- | The King's file when the game starts
kingFile     = 5         :: File

-- | The worth of any Pawn
pawnWorth    = 1         :: Value

-- | Function to create any piece, given its attributes of name, color, worth and location (formed out of its separate components, 
--   file and rank)
zipper       = \n c w file rank -> Piece { name  = n, color = c, worth = w, location = (file,rank)} 

-- | Function to form a location, given a file and rank
locZipper    = \f r -> (f,r)

-- | All files as a list, with the exception of the King's
nonKingFiles = filter (/=5) allFiles :: [File]

-- | Absolutely all files of all pieces
allFiles     = [1..8] :: [File]

-- | The rank of all black Pawns at the start of the game
bPawnsRank   = 7 :: Rank

-- | The rank of all white Pawns at the start of the game
wPawnsRank   = 2 :: Rank


{--
*****************************
*****************************
***HCHESS TYPE SYNONYMS******
*****************************
*****************************
--}

-- | An integer that describes a part of a location tuple, either rank or file 
type RankOrFile = Int

-- | The numeral representing the row a piece is in
type Rank = Int

-- | The numeral representing the file (column) a piece is in
type File = Int 

-- | A specific square on the board
type Location = (File, Rank)

-- | The value of the chess piece
type Value = Int

{--
*****************************
*****************************
******HCHESS DATA TYPES******
*****************************
*****************************
--}

-- | Type of pieces the pawns are (it is used as a phantom, for promotion rules)
data MINOR = MINOR

-- | Any piece that is not a pawn, except the King, is of this type (it is used as a phantom, for promotion rules)
data MAJOR = MAJOR

-- | The type describing the piece (King), which is the goal of chess
data ZIEL  = ZIEL

-- | Colors of the pieces on the chess board
data Color = BLACK | WHITE deriving (Show, Eq)

-- | Fundamental kinds of chess pieces in the game
data PieceType =  KING  | QUEEN  | ROOK  | BISHOP | KNIGHT | PAWN  deriving (Show, Eq)

-- | A typical chess piece
data Piece a = Piece {   name       :: PieceType,
                         color      :: Color, 
                         worth      :: Value, 
                         location   :: Location
                     }   deriving (Show, Eq)

-- | The type that we use to gather all chess types together
data BoardPiece = K (Piece ZIEL) | MI (Piece MINOR) | MA (Piece MAJOR) deriving (Eq)

-- | Designates the locations possible by any piece, at any one time
data PossibleMoves s = PossibleMoves s deriving (Show, Eq)

-- | The type that models the game moves our program can make based on the opponents responses.
--   The tree will be formed when we absorb pgn chess game files that have already been played.
--   The program will go down a path in the tree depending upon whether we can find a match 
--   with the move just made by the opponent. We deal with the move made (example, Nf3), the color
--   of the piece moving and the depth of the move in the tree
data GameTree m c d = DRAW m | MATE m d | STALEMATE m d | MOVE m c d [GameTree m c d] deriving (Show, Eq)

{--
*****************************
*****************************
****HCHESS TYPE CLASSES******
*****************************
*****************************
--}

-- | Minor pieces behaviour
class Minor a where
    moveBack :: a -> Bool

-- | Major pieces behaviour
class Major a where
    moveAnyDirection :: a -> Bool

-- | Behaviour out of the higher level board piece types
class Boarder a where
    paint  :: a -> Color
    locate :: a -> Location

-- | Ability of a piece to migrate from one square to another, either to capture or simply to move.
--   For a simple movement :  a piece to move and its intended location
--   For a piece capture:  a killer, a victim, a list of all victims captured so far
class Movable p where
    move    :: p -> Location -> p
    capture :: p -> p -> [p] -> (p, [p])

-- | Promotion of Minor pieces (i.e pawns). Major pieces are any pieces that are NOT pawns (and not the King)
class Promotable p t where
    promote :: p -> t -> t


{--
*****************************
*****************************
**HCHESS TYPECLASS INSTANCES*
*****************************
*****************************
--}


-- | Making only Minor pieces a member 
instance Minor MINOR where
    moveBack MINOR = False


-- | Making Major pieces a member 
instance Major MAJOR where
    moveAnyDirection MAJOR = True

-- | Making Kings a member 
instance Major ZIEL where
    moveAnyDirection ZIEL = True

-- | Instances of board pieces
instance Boarder BoardPiece where
    paint  (K p)   = color p
    paint  (MI p)  = color p
    paint  (MA p)  = color p
    locate (K p)   = location p
    locate (MI p)  = location p
    locate (MA p)  = location p

-- | Display the board, we are only interested in the pieces
instance Show BoardPiece where
    show (K  p)   = show p
    show (MI p)   = show p
    show (MA p)   = show p


-- | Functor instance
--   fmap :: (Functor f) => (a -> b) -> fa -> fb
instance Functor PossibleMoves where
    fmap f (PossibleMoves p) = PossibleMoves (f p)

-- | Applicativive instance
--   pure :: (Applicative f) => a -> f a
--   \<\*>  :: (Applicative f) => f(a -> b) -> f a -> f b
instance Applicative PossibleMoves where
    pure = PossibleMoves
    PossibleMoves f <*> PossibleMoves s = PossibleMoves (f s)
    
-- | Monadic instance
--   return :: (Monad m) => a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b
instance Monad PossibleMoves where
    return = PossibleMoves
    PossibleMoves s >>= f = f s
    

-- | Making all our pieces movable and the ability to capture other pieces
instance Movable (Piece a) where
    move p l        = p  { location = l } 
    capture k v l   = (k {location = location v}, (v:l))


-- | Promoting a Minor piece to Major piece
--   This is the only way we can promote in chess. 
instance (Minor a, Major b) => Promotable (Piece a) (Piece b) where
    promote p r = Piece { name     =  name r, 
                          color    =  color p, 
                          worth    =  worth r, 
                          location =  (fst $ location p, if (color p == BLACK) then uBound else lBound)
                         }
    