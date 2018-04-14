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


instance Movable Piece where
    move p@_  (x,y) = p { location = (x,y)} 
    capture k v l   = (k {location = location v}, (v:l))
