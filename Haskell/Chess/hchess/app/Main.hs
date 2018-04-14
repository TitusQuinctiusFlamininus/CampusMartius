module Main where

import Datatypes.ChessPlay
import Datatypes.ChessPieces


raisePiece :: Piece
raisePiece = let king = Piece {name=KING, color=BLACK, worth=1000, location=(5,8)} in 
    promote king BISHOP (6,1)

main :: IO ()
main = putStrLn $ show raisePiece
