

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

--pawns can be promoted to other pieces
class Promotable p where
    promote :: p -> PieceType -> Location -> p
    

--lets make pawns promotable
instance Promotable Piece where
    promote p t l = p {name = t, location = l} 