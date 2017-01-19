--Solving Sudoku in Haskell
type XLoc = Int
type YLoc = Int
type SValue = Int
type Found = Bool


data SudoCell = SudoCell (XLoc , YLoc , SValue, Found) deriving Show


--function to generate the board
--param1: the length of the board in terms of number of cells
--param2: the starting position of the lower left corner (you could use index 0 or 1 or whatever number you want) 
createBoard :: Int -> Int -> [SudoCell]
createBoard a p
	| p > a     = []
	| otherwise = (map (\x -> SudoCell (x, p, 0, False)) [1..a]) ++ createBoard a (p+1)

--function to give the list of SudoCells that are in the same row as the given SudoCell
sameRowCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameRowCells (SudoCell (_, y, _, _)) board = filter (\(SudoCell (_, a, _, _)) -> (y == a)) board

--function to give the list of SudoCells that are in the same column as the given SudoCell
sameColumnCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameColumnCells (SudoCell (x, _, _, _)) board = filter (\(SudoCell (a, _, _, _)) -> (x == a)) board


--function to check if SudoCell to the LEFT of the current cell has the same svalue
--Param1: The Reference Cell. We want to check all cells to the left of this one
--Param2: the list of SudoCells that have the same y value
--checkLeftForValue :: SudoCell -> [SudoCell] -> Bool
--checkLeftForValue SudoCell (x, y, s, _) =
