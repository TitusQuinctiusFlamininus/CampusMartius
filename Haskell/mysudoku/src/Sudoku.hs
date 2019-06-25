{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Sudoku where

import Data.List
import Data.Char
import Control.Lens.TH  
import Data.Foldable
import Data.Function                    ((&))
import Control.Lens.Type                (Lens')
import Control.Lens                     ((^.), (.~))
import qualified Data.Sequence as Seq

type FORWARD = String --We can proceed to the next cell
type BACK = String --we need to go back and remove the last used possible value in the last cell we processed
type STAY = String --we will keep processing the same cell
data Direction = FORWARD | BACK | STAY  deriving (Eq)  --are we processing the board forwards (toward 9,9) or backwards (to correct an earlier cell SValue assumption)
data SudoCell = SudoCell { _xLoc   :: Int, --the X coordinate of the cell
                           _yLoc   :: Int, --the Y coordinate of the cell
                           _sValue :: Int, --the sudoku value of the cell, a value between 1 and 9 (inclusive of 1 and 9)
                           _region :: Int, --the numbered region this cell belongs to; essentially a number given to a group of 9 cells (bottom left bottom = 1, bottom 
                           _poss   :: [Int], -- the possibilities (1 through 9) that act as an Svalue
                           _found  :: Bool --the indication if the cell's true sudoku value has been found, once this is set to true, then it will not and should not change
                         } deriving (Eq,Ord,Show)
                         
defaultCell = SudoCell {_xLoc = 0, _yLoc = 0, _sValue = 0, _region = 0, _poss = [1..9], _found = False}
                         
makeLenses ''SudoCell

--function to generate the board
--param: the board so far; usually, to start off, we send an empty board
createBoard :: [SudoCell] -> [SudoCell]
createBoard board
 | (length board) == 81     = board
 | otherwise                = createBoard $ fillInRegions (board ++ noRegionBoard)
                              where noRegionBoard = func <$> [1..9]
                                    func          = (\x -> (defaultCell & xLoc .~ x) & yLoc .~ y)
                                    y             = if (board == []) then 1 else ((length board) `div` 9)+1
     

--function to fill in the region or block for each cell
fillInRegions :: [SudoCell] -> [SudoCell]
fillInRegions celldata = zipWith (\s r -> s & region .~ r) celldata regiondata
 where regiondata              = concat . ((\l -> createRawRegionValues l) <$>) $ [[1..3],[4..6],[7..9]]
       createRawRegionValues   = \r -> concat . concat . replicate 3 . ((\x -> replicate 3 x) <$>) $ r


--function to give the list of SudoCells that are in the same region or column or row, depending on the LENS you pass in.
-- For example: If yLoc is the lens, then we filter all cells in the same ROW
-- if xLoc, then cells in the same column; if region, then cells with the same region, etc
sameAttributeCells :: Lens' SudoCell Int ->  SudoCell -> [SudoCell] -> [SudoCell]
sameAttributeCells focus cell = filter (\g -> g /= cell) . filter (\s -> (cell ^. focus == s ^. focus))
 
--function to update a board with the default sudoku values
setDefaultSudokuValues :: [(Int, Int, Int)] -> [SudoCell] -> [SudoCell]
setDefaultSudokuValues [] r                       = r
setDefaultSudokuValues def@((a,b,c):ys) (cell:xs) = 
 case (a == (cell ^. xLoc)) && (b == (cell ^. yLoc)) of
    True  -> ((cell & sValue .~ c) & found .~ True) : setDefaultSudokuValues ys xs
    False -> cell : setDefaultSudokuValues def xs


--function to convert string input for defaut cell values to a format we know about
inputToDefault :: String -> [(Int, Int, Int)]
inputToDefault ""         = []
inputToDefault " "        = []
inputToDefault (x:y:z:xs) =
    case x of
      ','  -> inputToDefault (y:z:xs)
      _    -> ((digitToInt x),(digitToInt y),(digitToInt z)) : inputToDefault xs

--SORTS THE BOARD so it appears from 1,1 through 9,9
sortBoard :: [SudoCell] -> [SudoCell]
sortBoard [] = []
sortBoard (x:xs) = sortBoard (filter (\y -> y < x) xs) ++ [x] ++ sortBoard (filter (\y -> y >= x) xs)

--Will tell you if the Possibility at the head of the list in the Sudocell, can be used as a value, considering the rows, columns and region values already
--set either by default or because we passed through those cells earlier in the program
isPossibilityOk :: SudoCell -> [SudoCell] -> Bool
isPossibilityOk cell@SudoCell{_poss = (x:xs)} board  =  all (x/=) forbiddenValues
 where  forbiddenValues = (\s -> s ^. sValue) <$> deciders
        deciders        = nub (sameAttributeCells yLoc cell board) ++ (sameAttributeCells xLoc cell board) ++ (sameAttributeCells region cell board)

--Will update the board given the index of the cell, the cell itself and the board
--param 1: the index
--param 2: the cell
--param 3: the board
updateAtIndex :: Int -> SudoCell -> [SudoCell] -> [SudoCell]
updateAtIndex index cell board = foldMap (:[]) $ Seq.update index cell $ Seq.fromList board



--STARTING FROM LOWER LEFT CORNER, AND FIND THE SUITABLE VALUES
--Param 1: The INDEX of the Cell we will deal with in this iteration
--Param 2: Direction of processing
--Param 3: The Sudoku board
solveSudoku :: Int -> Direction -> [SudoCell] -> [SudoCell]
solveSudoku index dir board
 | index == 81    = board
 | otherwise      = do
         case (cell ^. found) of
           False  -> case cell ^. poss of
                        [] -> solveSudoku (index-1) BACK $ updateAtIndex index (cell & poss .~ [1..9]) board
                        _  -> case isPossibilityOk cell board of
                              True  ->  solveSudoku (index+1) FORWARD $ updateAtIndex index ((cell & poss .~ (drop 1 $ cell ^. poss)) & sValue .~ head (cell ^. poss)) board
                              False ->  solveSudoku index STAY $ updateAtIndex index (cell & poss .~ (drop 1 $ cell ^. poss)) board
           True  -> case dir of 
                     BACK  -> solveSudoku (index-1) BACK board
                     _     -> solveSudoku (index+1) FORWARD board
   where cell = board !! index
                  

