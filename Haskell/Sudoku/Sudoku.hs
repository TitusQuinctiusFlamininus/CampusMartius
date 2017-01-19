--Solving Sudoku in Haskell
type XLoc = Int
type YLoc = Int
type SValue = Int
type Found = Bool


data SudoCell = SudoCell (XLoc , YLoc , SValue, Found) deriving (Eq, Show)


--function to generate the board
--param: the starting position of the lower left corner (1 will create cells, that have 1,1 as lower left corner of the board)
createBoard :: Int -> [SudoCell]
createBoard p
	| p > 9     = []
	| otherwise = (map (\x -> SudoCell (x, p, 0, False)) [1..9]) ++ createBoard (p+1)

--function to give the list of SudoCells that are in the same row as the given SudoCell, all except the row that is used as the reference request
sameRowCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameRowCells (SudoCell (a, b, c, d)) board = 
	let cells = filter (\(SudoCell (_, e, _, _)) -> (b == e)) board in
	filter (\g -> (g /= (SudoCell (a, b, c, d))) ) cells


--function to give the list of SudoCells that are in the same column as the given SudoCell, all except the row that is used as the reference request
sameColumnCells :: SudoCell -> [SudoCell] -> [SudoCell]
sameColumnCells (SudoCell (a, b, c, d)) board = 
	let cells = filter (\(SudoCell (e, _, _, _)) -> (a == e)) board in
	filter (\g -> (g /= (SudoCell (a, b, c, d))) ) cells


--function to check if SudoCell to the LEFT of the current cell has the same svalue
--Param1: The Reference Cell. We want to check all cells to the left of this one
--Param2: the list of SudoCells that have the same y value
--checkLeftForValue :: SudoCell -> [SudoCell] -> Bool
--checkLeftForValue SudoCell (x, y, s, _) =
