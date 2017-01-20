--Solving Sudoku in Haskell
type XLoc = Int     --the X coordinate of the cell
type YLoc = Int     --the Y coordinate of the cell
type SValue = Int   --the sudoku value of the cell, a value between 1 and 9 (inclusive of 1 and 9)
type Region = Int   --the numbered region this cell belongs to; essentially a number given to a group of 9 cells (bottom left bottom = 1, bottom middle bottom = 2, bottom right bottom = 3, middle left middle = 4, dead center = 5, middle right middle = 6, upper left upper = 7, upper middle upper = 8, upper right upper = 9 )
type Found = Bool   --the indication if the cell's true sudoku value has been found, once this is set to true, then it will not and should not change


data SudoCell = SudoCell (XLoc , YLoc , SValue, Region, Found) deriving (Eq, Show)


--function to generate the board
--param: the starting position of the lower left corner (1 will create cells, that have 1,1 as lower left corner of the board)
createBoard :: Int -> [SudoCell]
createBoard p
 | p > 9     = []
 | otherwise = 
 let noRegionBoard = (map (\x -> SudoCell (x, p, 0, 0, False)) [1..9]) ++ createBoard (p+1) in 
 fillInRegions noRegionBoard

--PRIVATE FUNCTION: USED IN THE createBoard Function to fill in the regions data for each cell
--function to fill in the region or block for each cell
fillInRegions :: [SudoCell] -> [SudoCell]
fillInRegions celldata =
 let regiondata = concat $ (concat $ replicate 3 $ map (\x -> replicate 3 x) [1,2,3]) ++ (concat $ replicate 3 $ map (\x -> replicate 3 x) [4,5,6])  ++ (concat $ replicate 3 $ map (\x -> replicate 3 x) [7,8,9])
 in zipWith (\(SudoCell (a, b, c, _, d)) r -> SudoCell (a, b, c, r, d)) celldata regiondata

--function to give the list of SudoCells that are in the same row as the given SudoCell, all except the row that is used as the reference request
sameRowCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameRowCells (SudoCell (a, b, c, d, f)) board = 
 let cells = filter (\(SudoCell (_, e, _, _, _)) -> (b == e)) board in
 filter (\g -> (g /= (SudoCell (a, b, c, d, f))) ) cells


--function to give the list of SudoCells that are in the same column as the given SudoCell, all except the row that is used as the reference request
sameColumnCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameColumnCells (SudoCell (a, b, c, d, f)) board = 
 let cells = filter (\(SudoCell (e, _, _, _, _)) -> (a == e)) board in
 filter (\g -> (g /= (SudoCell (a, b, c, d, f))) ) cells


--function to check if SudoCell to the LEFT of the current cell has the same svalue
--Param1: The Reference Cell. We want to check all cells to the left of this one
--Param2: the list of SudoCells that have the same y value
--checkLeftForValue :: SudoCell -> [SudoCell] -> Bool
--checkLeftForValue SudoCell (x, y, s, _) =
