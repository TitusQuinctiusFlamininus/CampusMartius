module Datatypes.ChessConstants where

--the rook, knight and bishop worths in a list
rkbWorths    = [5, 3, 3] :: [Int]


--the highest rank or file any piece can reach
uBound = 8 :: Int

--the lowest rank or file any piece can reach
lBound = 1 :: Int

--a list describing a full spectrum of location indexes (from 1 through 8, in either direction)
boardSpan = [lBound..uBound]