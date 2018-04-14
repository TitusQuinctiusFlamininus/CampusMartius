

module Datatypes.ChessPlay where

import Datatypes.ChessPieces

--the gameboard, as a list of coordinates on an 8x8 
type Board = [Location]


--typeclass embodying the ability of a piece to translocate from one square to another
--either to capture or simply to move
class Movable p where
    --a piece to move and its intended location
    move    :: p -> Location -> p
    -- a killer, a victim, a list of all victims captured so far
    capture :: p -> p -> [p] -> (p, [p])

--lets make all our pieces movable and the ability to capture other pieces
instance Movable Piece where
    move p@_  (x,y) = p { location = (x,y)} 
    capture k v l   = (k {location = location v}, (v:l))

--typeclass representing minor pieces (i.e pawns). Major pieces are any pieces that are NOT pawns, since we c
class Promotable p where
    promote :: p -> PieceType -> Location -> Piece
    
instance Promotable Piece where
    promote p@Piece{name= PAWN} QUEEN l = p {name= QUEEN, location = l}
    promote p@Piece{name= PAWN} ROOK l = p {name= ROOK, location = l}
    promote p@Piece{name= PAWN} BISHOP l = p {name= BISHOP, location = l}
    promote p@Piece{name= PAWN} KNIGHT l = p {name= KNIGHT, location = l}