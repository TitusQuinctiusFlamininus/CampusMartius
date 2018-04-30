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
anyPieceMoves :: Location -> PieceType -> [Location] -> Moves [Location]
anyPieceMoves l t c =  (<-?->) l >=> (.<->.) >=> (<-!->) c $ t 
