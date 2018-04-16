module Main where

import Datatypes.ChessPlay
import Datatypes.ChessPieces


promoteThePiece :: Piece Minor -> Piece Major -> Piece Major
promoteThePiece =  promote


promoted :: Piece Major
promoted = Piece {name=ROOK, color=BLACK, worth=5, location=(0,0)}

promoter :: Piece Minor
promoter = Piece {name=PAWN, color=BLACK, worth=1, location=(6,2)}

main :: IO ()
main = putStrLn $ show $ promoteThePiece promoter promoted
