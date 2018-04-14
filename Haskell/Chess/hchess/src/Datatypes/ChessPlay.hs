module Datatypes.ChessPlay where

import Datatypes.ChessPieces

--the gameboard, as a list of coordinates on an 8x8 
type Board = [Location]


--typeclass embodying the ability of a piece to translocate from one square to another
--either to capture or simply to move
class Movable p where
    move    :: p -> Location -> p
    capture :: p -> p -> (p, [p])
    
