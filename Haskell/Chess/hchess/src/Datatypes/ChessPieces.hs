module Datatypes.ChessPieces where

import Datatypes.ChessTypes 
import Datatypes.ChessConstants

import Data.List     (zipWith5)

--function to create the Kings
allKings :: [Piece ZIEL]
allKings = zipWith (\c y -> Piece { name=KING, color=c, worth=1000, location=(5,y)} ) [BLACK, WHITE] [8,1]

--function to create all minor pieces, white and black (pawns)       
allMinorPieces :: [Piece MINOR]
allMinorPieces = pawner BLACK bPawnsRank ++ pawner WHITE wPawnsRank

--function to create pawns of a certain color, placed on a specific row
pawner :: Color -> Rank -> [Piece MINOR]
pawner c r = zipWith3 (\c x y -> Piece { name=PAWN, color=c, worth=1, location=(x,y)}) (mult c) allFiles (mult r)

--function to make all major pieces, except the king
makeMajors :: Color -> Rank -> [Piece MAJOR]
makeMajors c r = zipWith5 zipper nameList (mult c) worthList nonKingFiles (mult r)

--function to create all major pieces, white and black (Rooks, Knights, Bishops, Queens, Kings)
allMajorPieces :: [Piece MAJOR]
allMajorPieces = makeMajors BLACK 8 ++ makeMajors WHITE 1

--function to gather all pieces and place them on the chess board
setChessBoard :: [Board]
setChessBoard = kings++minors++majors
                where kings   = map (\k  -> K k   ) allKings
                      minors  = map (\mi -> MI mi ) allMinorPieces
                      majors  = map (\ma -> MA ma ) allMajorPieces
                     
                

