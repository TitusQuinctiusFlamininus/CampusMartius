module Main where

import Datatypes.ChessPlay
import Datatypes.ChessPieces


main :: IO ()
main = putStrLn $ show $ promote (head allMinorPieces) (head allMajorPieces)
       --putStrLn $ show $ promote (head allMajorPieces) (head allMajorPieces)
       --putStrLn $ show $ promote (head allMajorPieces) (head allMinorPieces)
