module Datatypes.ChessPieces where

import Datatypes.ChessTypes 
import Datatypes.ChessConstants
import Utilities.ChessUtils

import Data.List     (zipWith5, nub)

--function to create the Kings
allKings :: [Piece ZIEL]
allKings = zipWith (\c y -> Piece { name=KING, color=c, worth=1000, location=(5,y)} ) [BLACK, WHITE] [uBound,lBound]

--function to create all minor pieces, white and black (pawns)       
allMinorPieces :: [Piece MINOR]
allMinorPieces = pawner BLACK bPawnsRank ++ pawner WHITE wPawnsRank

--function to create pawns of a certain color, placed on a specific row
pawner :: Color -> Rank -> [Piece MINOR]
pawner c r = zipWith3 (\c x y -> Piece { name=PAWN, color=c, worth=1, location=(x,y)}) ((<->) 8 c) allFiles ((<->) 8 r)

--function to make all major pieces, except the king
makeMajors :: Color -> Rank -> [Piece MAJOR]
makeMajors c r = zipWith5 zipper nameList ((<->) 8 c) worthList nonKingFiles ((<->) 8 r)

--function to create all major pieces, white and black (Rooks, Knights, Bishops, Queens, Kings)
allMajorPieces :: [Piece MAJOR]
allMajorPieces = makeMajors BLACK uBound ++ makeMajors WHITE lBound

--function to gather all pieces and place them on the chess board
setChessBoard :: [BoardPiece]
setChessBoard = nub $ concat [[K k, MI mi, MA ma] | k <- allKings, mi <- allMinorPieces, ma <- allMajorPieces]
                
                

