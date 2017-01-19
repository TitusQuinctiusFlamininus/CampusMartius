--Solving Sudoku in Haskell
type XLoc = Int
type YLoc = Int
type SValue = Int
type Region = Int
type Found = Bool


data SudoCell = SudoCell (XLoc , YLoc , SValue, Region, Found) deriving (Eq, Show)


--function to generate the board
--param: the starting position of the lower left corner (1 will create cells, that have 1,1 as lower left corner of the board)
createBoard :: Int -> [SudoCell]
createBoard p
	| p > 9     = []
	| otherwise = (map (\x -> SudoCell (x, p, 0, 0, False)) [1..9]) ++ createBoard (p+1)


--function to fill in the region or block for each cell
fillInRegion :: [SudoCell] -> [SudoCell]
fillInRegion celldata = 
	let regiondata = concat $ ((replicate 3 ([1,1,1]++[2,2,2]++[3,3,3])) ++ (replicate 3 ([4,4,4]++[5,5,5]++[6,6,6]))  ++ (replicate 3 ([7,7,7]++[8,8,8]++[9,9,9]))) in
	zipWith (\(SudoCell (a, b, c, _, d)) r -> SudoCell (a, b, c, r, d)) celldata regiondata

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
