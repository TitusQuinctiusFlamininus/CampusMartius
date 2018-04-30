module Main where

import Datatypes.ChessPlay
import Datatypes.ChessPieces
import Datatypes.ChessTypes

import Control.Monad ((>=>))

main :: IO ()
main = do mapM_ (putStrLn . show) setChessBoard
          putStrLn "PROMOTING A PAWN TO A ROOK"    
          putStrLn $ show $ promote (head allMinorPieces) (head allMajorPieces)
          --putStrLn $ show $ promote (head allMajorPieces) (head allMajorPieces)
          --putStrLn $ show $ promote (head allMajorPieces) (head allMinorPieces)


--function to determine the actual moves possible for any piece, from any current position
--First Parameter  ::  The location of the piece that wants to make a move
--Second Parameter ::  The list of locations of all pieces of the same color, as the piece that wants to make a move
--Third Parameter  ::  The PieceType of the piece that wants to make a move
anyPieceMoves :: Location -> [Location] -> PieceType -> Moves [Location]
anyPieceMoves l c =  (<-?->) l >=> (.<->.) >=> (<-!->) c
