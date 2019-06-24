module Main where

import Data.List (nub)
import Sudoku

main = do
 let hollowboard = createBoard []
 putStrLn "***********************************************************************************************************"
 putStrLn "===================================WELCOME TO HASKELL SUDOKU==============================================="
 putStrLn "***********************************************************************************************************"
 putStrLn ""
 putStrLn "Enter values already solved on the board in the format: 123,456 etc...."
 putStrLn " For example: 123,456 would imply: 2nd cell in 1st column has value 3, 5th cell in the 4th column has value 6, etc "
 putStrLn "(Note: Traverse the board from lower left cell, moving left to right for the bottom row, then the next row, etc....)"
 inputValues <- getLine
 let defaultInput = inputToDefault inputValues
 let bbp = setDefaultSudokuValues (inputToDefault inputValues) hollowboard
 let finalsolution = nub (solveSudoku 0 FORWARD bbp)
 --putStrLn (show (length finalsolution))
 putStrLn " elements."
 putStrLn " "
 putStrLn (show $ sortBoard finalsolution)
