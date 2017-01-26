import Data.List
import Data.Char
import qualified Data.Sequence as Seq
import Data.Foldable

--Solving Sudoku in Haskell
type XLoc = Int     --the X coordinate of the cell
type YLoc = Int     --the Y coordinate of the cell
type SValue = Int   --the sudoku value of the cell, a value between 1 and 9 (inclusive of 1 and 9)
type Region = Int   --the numbered region this cell belongs to; essentially a number given to a group of 9 cells (bottom left bottom = 1, bottom middle bottom = 2, bottom right bottom = 3, middle left middle = 4, dead center = 5, middle right middle = 6, upper left upper = 7, upper middle upper = 8, upper right upper = 9 )
type Found = Bool   --the indication if the cell's true sudoku value has been found, once this is set to true, then it will not and should not change
type FORWARD = String --We can proceed to the next cell
type BACK = String --we need to go back and remove the last used possible value in the last cell we processed

data Possibilities = Possibilities [Int] deriving (Eq, Show, Ord)   --the possible values a cell can have.
data Direction = FORWARD | BACK  deriving (Eq)  --are we processing the board forwards (toward 9,9) or backwards (to correct an earlier cell SValue assumption)
data SudoCell = SudoCell (XLoc , YLoc , SValue, Region, Possibilities, Found) deriving (Eq, Show, Ord)   --A typical Sudoku Cell


--function to generate the board
--param: the board so far; usually, to start off, we send an empty board
createBoard :: [SudoCell] -> [SudoCell]
createBoard board
 | (length board) == 81     = board
 | otherwise                = 
     let y = if (length board == 0) then 1 else ((length board) `div` 9)+1
         noRegionBoard = map (\x -> SudoCell (x, y, 0, 0, Possibilities [1..9], False)) [1..9]
         rdataInc = fillInRegions (board ++ noRegionBoard) in
     createBoard rdataInc 

--PRIVATE FUNCTION: Used in the createBoard Function to fill in the regions data for each cell
--function to fill in the region or block for each cell
fillInRegions :: [SudoCell] -> [SudoCell]
fillInRegions celldata =
 let regiondata = (createRawRegionValues [1..3]) ++ (createRawRegionValues [4..6]) ++ (createRawRegionValues [7..9])
 in zipWith (\(SudoCell (a, b, c, _, p, d)) r -> SudoCell (a, b, c, r, p, d)) celldata regiondata

--PRIVATE FUNCTION: Used in the fillInRegions function to give back a list like this, given [1,2,3]: [1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3,1,1,1,2,2,2,3,3,3]
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
isPossibilityOk (SudoCell (a, b, c, d, Possibilities(x:xs), f)) board
 | Possibilities(x:xs) == Possibilities[]   = False
	| otherwise                                =
       let deciders = nub (sameRowCells (SudoCell (a, b, c, d, Possibilities(x:xs), f)) board) ++ (sameColumnCells (SudoCell (a, b, c, d, Possibilities(x:xs), f)) board) ++ (sameRegionCells (SudoCell (a, b, c, d, Possibilities(x:xs), f)) board)
           forbiddenValues = map (\(SudoCell (_, _, s, _, _, _)) -> s) deciders in
       all (x/=) forbiddenValues

--MAIN FUNCTION TO GO THROUGH EACH ELEMENT OF THE BOARD, STARTING FROM LOWER LEFT CORNER, AND FIND THE SUITABLE VALUES
-- Param 1: The INDEX of the Cell we will deal with in this iteration
-- Param 2: The DIRECTION we last used: If the last round involved a BACK direction, then it means we had a problem with a cell ahead, and we need to adjust the head-value, in the list of possible values the current cell can use; Otherwise, FORWARD means can use the present value (head of possible values list) and try out new possibilities in going forward
--Param 3: The Sudoku board 
--Param 4: A Stack that contains the elements of the board as you process them; they are popped or pushed back in
solveSudoku :: Int -> Direction -> [SudoCell] -> [SudoCell] -> [SudoCell]
solveSudoku index dir board sudostack
 | index == (length board)    = sudostack
 | dir == FORWARD = do {
 	                      let (SudoCell (a, b, c, d, Possibilities(x:xs), f)) = board !! index in
                             if (f /= True)
                                then let newboard         = foldMap (:[]) (Seq.update index (SudoCell (a, b, x, d, Possibilities(x:xs), f)) $ Seq.fromList board)
                                         falsevaluestack  = ((SudoCell (a, b, x, d, Possibilities(x:xs), f)) : sudostack) in
                                     solveSudoku (index+1) FORWARD newboard falsevaluestack
                             else
                                     let alreadysetstack       = ((SudoCell (a, b, c, d, Possibilities(x:xs), f)):sudostack) in
                                     solveSudoku (index+1) FORWARD board alreadysetstack
                       }
--WE NEED TO TAKE CARE OF THIS CASE: 
-- <- IMPORTANT: Need a check here to see if DIRECTION WAS BACK...if it is then you have to move back again to a cell that is NOT true! because you could have gotten here from a future cell that wants to go back to the last NON-TRUE cell.... So, you need to check if the direction was BACK......if so, append the current cell on the stack and use the BACKWARD flag


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
	--putStrLn (show defaultInput)
	let partialboard = setDefaultSudokuValues defaultInput hollowboard
	--putStrLn (show partialboard)
	let readyboard = partialboard ++ postDefault partialboard hollowboard
	--putStrLn (show readyboard)
	let finally = nub (solveSudoku 0 FORWARD readyboard [])
	putStrLn " "
	putStr "The Sudoku Board has "
	putStr (show (length finally))
	putStrLn " elements."
	putStrLn " "
	putStrLn (show $ sortBoard finally)