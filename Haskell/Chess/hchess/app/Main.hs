module Main where

import Datatypes.ChessPlay
import Datatypes.ChessPieces


main :: IO ()
main = do mapM_ (putStrLn . show) setChessBoard
          putStrLn "PROMOTING A PAWN TO A ROOK"    
          putStrLn $ show $ promote (head allMinorPieces) (head allMajorPieces)
          --putStrLn $ show $ promote (head allMajorPieces) (head allMajorPieces)
          --putStrLn $ show $ promote (head allMajorPieces) (head allMinorPieces)
