import Data.List
import Data.Char
import qualified Data.Sequence as Seq
import Data.Foldable

--Solving Sudoku in Haskell
type XLoc = Int     --the X coordinate of the cell
type YLoc = Int     --the Y coordinate of the cell
type SValue = Int   --the sudoku value of the cell, a value between 1 and 9 (inclusive of 1 and 9)
type Region = Int   --the numbered region this cell belongs to; essentially a number given to a group of 9 cells (bottom left bottom = 1, bottom middle bottom = 2, bottom right bottom = 3, middle left middle = 4, dead center = 5, middle right middle = 6, upper left upper = 7, upper middle upper = 8, upper right upper = 9 )
type Poss = [Int] -- the possibilities (1 through 9) that act as an Svalue
type Found = Bool   --the indication if the cell's true sudoku value has been found, once this is set to true, then it will not and should not change
type FORWARD = String --We can proceed to the next cell
type BACK = String --we need to go back and remove the last used possible value in the last cell we processed
type STAY = String --we will keep processing the same cell 
data Direction = FORWARD | BACK | STAY  deriving (Eq)  --are we processing the board forwards (toward 9,9) or backwards (to correct an earlier cell SValue assumption)
data SudoCell = SudoCell (XLoc , YLoc , SValue, Region, Poss, Found) deriving (Eq, Show, Ord)   --A typical Sudoku Cell


--function to generate the board
--param: the board so far; usually, to start off, we send an empty board
createBoard :: [SudoCell] -> [SudoCell]
createBoard board
 | (length board) == 81     = board
 | otherwise                = 
     let y = if (board == []) then 1 else ((length board) `div` 9)+1
         noRegionBoard = map (\x -> SudoCell (x, y, 0, 0, [1..9], False)) [1..9]
         rdataInc = fillInRegions (board ++ noRegionBoard) in
     createBoard rdataInc 

--function to fill in the region or block for each cell
fillInRegions :: [SudoCell] -> [SudoCell]
fillInRegions celldata =
 let regiondata = (createRawRegionValues [1..3]) ++ (createRawRegionValues [4..6]) ++ (createRawRegionValues [7..9])
 in zipWith (\(SudoCell (a, b, c, _, p, d)) r -> SudoCell (a, b, c, r, p, d)) celldata regiondata

--Used in the fillInRegions function to give back a list like this, given [1,2,3]: [1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3]
-- the value 3 used in the function is the height of any 1 region on the sudoku board, a region comprising of a 3x3 square of sudoku cells
createRawRegionValues :: [Int] -> [Int]
createRawRegionValues r = concat $ concat $ replicate 3 $ map (\x -> replicate 3 x) r

--function to give the list of SudoCells that are in the same row as the given SudoCell, all except the row that is used as the reference request
sameRowCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameRowCells (SudoCell (a, b, c, d, p, f)) board = 
 let cells = filter (\(SudoCell (_, e, _, _, _, _)) -> (b == e)) board in
 filter (\g -> (g /= (SudoCell (a, b, c, d, p, f))) ) cells


--function to give the list of SudoCells that are in the same column as the given SudoCell, all except the row that is used as the reference request
sameColumnCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameColumnCells (SudoCell (a, b, c, d, p, f)) board = 
 let cells = filter (\(SudoCell (e, _, _, _, _, _)) -> (a == e)) board in
 filter (\g -> (g /= (SudoCell (a, b, c, d, p, f))) ) cells

--function to give the list of SudoCells that are in the same region as the given SudoCell, all except the row that is used as the reference request
sameRegionCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameRegionCells (SudoCell (a, b, c, d, p, f)) board = 
 let cells = filter (\(SudoCell (_, _, _, r, _, _)) -> (d == r)) board in
 filter (\g -> (g /= (SudoCell (a, b, c, d, p, f))) ) cells

--function to update a board with the default sudoku values
setDefaultSudokuValues :: [(Int, Int, Int)] -> [SudoCell] -> [SudoCell]
setDefaultSudokuValues ((a,b,c):ys) ((SudoCell (d, e, f, g, h, i)):xs)
 | a == d && b == e   = (SudoCell (d, e, c, g, h, True)) : setDefaultSudokuValues ys xs
 | otherwise          = (SudoCell (d, e, f, g, h, i)) : setDefaultSudokuValues ((a,b,c):ys) xs
setDefaultSudokuValues [] _ = []

--function to finally add the missing cells from the original board that do not have any default values
postDefault :: [SudoCell] -> [SudoCell] -> [SudoCell]
postDefault defaultValues origBoard = 
 let indexToUse = length defaultValues in
 filter (\e -> (e `elemIndex` origBoard) >= (Just indexToUse)) origBoard

--function to convert string input for defaut cell values to a format we know about
inputToDefault :: String -> [(Int, Int, Int)]
inputToDefault "" = []
inputToDefault (x:y:z:xs)
 | x == ',' = inputToDefault (y:z:xs)
 | otherwise = ((digitToInt x),(digitToInt y),(digitToInt z)) : inputToDefault xs

--SORTS THE BOARD so it appears from 1,1 through 9,9
sortBoard :: [SudoCell] -> [SudoCell]
sortBoard [] = []
sortBoard (x:xs) = sortBoard (filter (\y -> y < x) xs) ++ [x] ++ sortBoard (filter (\y -> y >= x) xs)

--Will tell you if the Possibility at the head of the list in the Sudocell, can be used as a value, considering the rows, columns and region values already
--set either by default or because we passed through those cells earlier in the program
isPossibilityOk :: SudoCell -> [SudoCell] -> Bool
isPossibilityOk cell board =
 let deciders = nub (sameRowCells cell board) ++ (sameColumnCells cell board) ++ (sameRegionCells cell board)
     (SudoCell (_, _, _, _, (x:xs), _)) = cell     
     forbiddenValues = map (\(SudoCell (_, _, s, _, _, _)) -> s) deciders in
     all (x/=) forbiddenValues

--Will update the board given the index of the cell, the cell itself and the board
--param 1: the index
--param 2: the cell 
--param 3: the board
updateAtIndex :: Int -> SudoCell -> [SudoCell] -> [SudoCell] 
updateAtIndex index cell board = foldMap (:[]) (Seq.update index cell $ Seq.fromList board)


--STARTING FROM LOWER LEFT CORNER, AND FIND THE SUITABLE VALUES
--Param 1: The INDEX of the Cell we will deal with in this iteration
--Param 2: Direction of processing
--Param 3: The Sudoku board 
solveSudoku :: Int -> Direction -> [SudoCell] -> [SudoCell]
solveSudoku index dir board
 | index == 81    = board
 | otherwise = do {
      let (SudoCell (a, b, c, d, p, f)) = board !! index in
         if (f /= True) then
               if (p/=[]) then 
                      if isPossibilityOk (SudoCell (a, b, c, d, p, f)) board then
                         let goodcellboard         = updateAtIndex index (SudoCell (a, b, (head p), d, (drop 1 p), f)) board in
                             solveSudoku (index+1) FORWARD goodcellboard
                      else let tryagainboard       = updateAtIndex index (SudoCell (a, b, 0, d, (drop 1 p), f)) board in
                             solveSudoku index STAY tryagainboard
               else let badcellboard               = updateAtIndex index (SudoCell (a, b, 0, d, [1..9], f)) board in
                             solveSudoku (index-1) BACK badcellboard
         else 
               if dir == BACK then 
                  solveSudoku (index-1) BACK board
               else solveSudoku (index+1) FORWARD board
                  }


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
	let partialboard = setDefaultSudokuValues (inputToDefault inputValues) hollowboard
	--board before processing
	let bbp = partialboard ++ postDefault partialboard hollowboard
	--putStr (show bbp)
	let finalsolution = nub (solveSudoku 0 FORWARD bbp) 
	putStrLn (show (length finalsolution))
	putStrLn " elements."
	putStrLn " "
	putStrLn (show $ sortBoard finalsolution)