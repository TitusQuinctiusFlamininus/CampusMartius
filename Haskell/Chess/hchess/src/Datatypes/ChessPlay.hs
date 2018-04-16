{-# LANGUAGE MultiParamTypeClasses #-}


module Datatypes.ChessPlay where

import Datatypes.ChessTypes

--typeclass embodying the ability of a piece to translocate from one square to another
--either to capture or simply to move
--for move:  --a piece to move and its intended location
--for capture:  -- a killer, a victim, a list of all victims captured so far
class Movable p where
    move    :: p -> Location -> p
    capture :: p -> p -> [p] -> (p, [p])

--lets make all our pieces movable and the ability to capture other pieces
instance Movable (Piece a)where
    move p (x,y)    = p { location = (x,y)} 
    capture k v l   = (k {location = location v}, (v:l))

--typeclass representing minor pieces (i.e pawns). Major pieces are any pieces that are NOT pawns, since we c
class Promotable p t where
    promote :: p -> t -> Piece a

--promoting pawns to different major pieces
instance (Minor a, Major b) => Promotable (Piece a) (Piece b) where
    promote p@Piece {name=PAWN} r@Piece{name=ROOK}   = 
     Piece {name=ROOK, color=(color p), worth=(worth r),   location=((fst $ location p), (if (color p == BLACK) then 1 else 8))}
    promote p@Piece {name=PAWN} r@Piece{name=KNIGHT} = 
     Piece {name=KNIGHT, color=(color p), worth=(worth r), location=((fst $ location p), (if (color p == BLACK) then 1 else 8))}
    promote p@Piece {name=PAWN} r@Piece{name=BISHOP} = 
     Piece {name=BISHOP, color=(color p), worth=(worth r), location=((fst $ location p), (if (color p == BLACK) then 1 else 8))}
    promote p@Piece {name=PAWN} r@Piece{name=QUEEN}  = 
     Piece {name=QUEEN, color=(color p), worth=(worth r),  location=((fst $ location p), (if (color p == BLACK) then 1 else 8))}
    